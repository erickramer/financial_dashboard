taxes = function(df, standard_deduction = T, other_deductions = 0){
  
  if(!('f01k_contribution' %in% colnames(df))){
    df = df %>% mutate(f01k_contribution = 36000)
  }
  
  if(!('ira_contribution' %in% colnames(df))){
    df = df %>% mutate(ira_contribution = 11000)
  }
  
  deductions = if(standard_deduction) 12000 + other_deductions else
    other_deductions
  
  taxes = df %>%
    mutate(income = income1 + income2) %>%
    mutate(taxable_income = income - f01k_contribution - ira_contribution -
             deductions) %>%
    mutate(federal_tax = map_dbl(taxable_income, federal_taxes)) %>%
    mutate(state_tax = map_dbl(taxable_income, state_taxes)) %>%
    mutate(ss_tax = map_dbl(taxable_income, ss_taxes)) %>%
    mutate(medicare_tax = map_dbl(taxable_income, medicare_taxes)) %>%
    mutate(total_tax = federal_tax + state_tax + ss_tax + medicare_tax) %>%
    mutate(net_income = income - total_tax) %>%
    mutate(effective_tax_rate = 1 - net_income / income)
  
  taxes
}

contributions = function(income_after_taxes, fc = NULL, sr = NULL){
  
  if(!is.null(fc)){
    income_after_taxes = income_after_taxes %>%
      mutate(f01k_match_pct = fc)
  } else if(is.null(fc) && 
            !("f01k_match_pct" %in% colnames(income_after_taxes))){
    income_after_taxes = income_after_taxes %>%
      mutate(f01k_match_pct = 0)
  }
  
  if(!is.null(sr)){
    income_after_taxes = income_after_taxes %>%
      mutate(savings_rate = sr)
  }
  
  income_after_taxes %>%
    mutate(saving_goal = income * savings_rate) %>%
    mutate(f01k_match = min(income*f01k_match_pct, f01k_contribution)) %>%
    mutate(f01k_total_contribution = f01k_match + f01k_contribution) %>%
    mutate(taxable_contribution = saving_goal - f01k_contribution - 
             f01k_match - ira_contribution) %>%
    mutate(net_income_after_contributions = net_income - 
             f01k_contribution - 
             ira_contribution - 
             taxable_contribution) %>%
    select(year,
           income, 
           total_tax,
           net_income, 
           f01k_contribution, 
           f01k_total_contribution, 
           ira_contribution,
           taxable_contribution,
           net_income_after_contributions) %>%
    mutate(net_monthly_income = net_income_after_contributions / 12,
           max_monthly_rent = net_monthly_income / 3)
}

.recursive_tax = function(income, brackets, rates, tax = 0){
  if(brackets[1] != 0 || length(brackets) != length(rates))
    stop("These brackets or rates ain't right")
  
  if(income <= 0){
    tax
  }
  else{
    
    n = length(brackets)
    
    if(income > brackets[n]){ 
      tax = tax + rates[n] * (income - brackets[n])
      income = brackets[n]
    }
    
    .recursive_tax(income, 
                   brackets[1:(n-1)], 
                   rates[1:(n-1)], 
                   tax)
  }
}

federal_taxes = function(taxable_income){
  
  if(taxable_income < 0)
    stop("Income must be positive")
  
  brackets = c(0,
               19050,
               77400,
               165000,
               315000,
               400000,
               600000)
  
  rates = c(0.1,
            0.15,
            0.25,
            0.28,
            0.33,
            0.35,
            0.37)
  
  .recursive_tax(taxable_income, brackets, rates)
}

state_taxes = function(taxable_income){
  
  if(taxable_income < 0)
    stop("Income must be positive")
  
  brackets = c(0,
               16030,
               38002,
               59978,
               83258,
               105224,
               537500,
               644998,
               1000000,
               1074996)
  
  rates = c(0.01,
            0.02,
            0.04,
            0.06,
            0.08,
            0.093,
            0.103,
            0.113,
            0.123,
            0.133)
  
  .recursive_tax(taxable_income, brackets, rates)
  
}

medicare_taxes = function(taxable_income){
  
  if(taxable_income < 0)
    stop("Income must be positive")
  
  brackets = c(0,
               250000)
  
  rates = c(0.0145,
            0.0235)
  
  .recursive_tax(taxable_income, brackets, rates)
}

ss_taxes = function(taxable_income){
  
  brackets = c(0,
               128400 * 2)
  
  rates = c(0.062,
            0) # no tax above SS limit
  
  .recursive_tax(taxable_income, brackets, rates)
}


