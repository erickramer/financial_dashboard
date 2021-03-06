calc_taxes = function(pretax_contributions, 
                 standard_deduction = T, 
                 other_deductions = 0){
  
  loginfo("Calculating taxes")

  deductions = if(standard_deduction) 12000 + other_deductions else
    other_deductions
  
  taxes = pretax_contributions %>%
    mutate(income_taxable = income - contribution_401k - 
             contribution_ira - deductions) %>%
    mutate(tax_federal = map_dbl(income_taxable, federal_taxes)) %>%
    mutate(tax_state = map_dbl(income_taxable, state_taxes)) %>%
    mutate(tax_ss = map_dbl(income_taxable, ss_taxes)) %>%
    mutate(tax_medicare = map_dbl(income_taxable, medicare_taxes)) %>%
    mutate(tax_total = tax_federal + tax_state + tax_ss + tax_medicare) %>%
    mutate(income_after_taxes = income - tax_total) %>%
    mutate(tax_effective_rate = tax_total / income) %>%
    select(year,
           starts_with("income"), 
           starts_with("contribution"), 
           starts_with("tax"))
  
  taxes
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


