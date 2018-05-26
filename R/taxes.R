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


