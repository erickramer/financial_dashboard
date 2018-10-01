MAX_401K = 18500 * 2
MAX_IRA = 5500 * 2

calc_pretax_contributions = function(df, 
                                savings_rate = 0.5, 
                                match_401k = 0){
  
  loginfo("Calculating pretax contributions")
  
  incomes = df %>%
    mutate(income = income1 + income2) %>%
    select(-income1, -income2) %>%
    mutate(savings_remaining = income * savings_rate) %>%
    mutate(income_remaining = income) 
  
  contribution_401 = incomes %>%
    mutate(contribution_401k_employee = pmin(income_remaining, 
                                   savings_remaining, 
                                   MAX_401K)) %>%
    mutate(contribution_401k_employer = min(income * match_401k, contribution_401k_employee)) %>%
    mutate(contribution_401k = contribution_401k_employer + 
             contribution_401k_employee) %>%
    mutate(income_remaining = income_remaining - contribution_401k_employee,
           savings_remaining = savings_remaining - contribution_401k)
  
  contribution_ira = contribution_401 %>% 
    mutate(contribution_ira = pmin(income_remaining, 
                                    savings_remaining, 
                                    MAX_IRA)) %>%
    mutate(income_remaining = income_remaining - contribution_ira,
           savings_remaining = savings_remaining - contribution_ira)
  
  contribution_ira %>%
    select(-savings_remaining, -income_remaining)
}

calc_posttax_contributions = function(taxes, savings_rate = 0.5){
  
  loginfo("Calculating post contributions")
  
  contributions = taxes %>%
    mutate(income_remaining = income - tax_total - 
             contribution_401k_employee - contribution_ira) %>%
    mutate(savings_remaining = income * savings_rate - 
             contribution_401k_employee - contribution_ira) %>%
    mutate(contribution_taxable = pmin(income_remaining, 
                                       savings_remaining)) %>%
    mutate(income_take_home = income_remaining - contribution_taxable) %>%
    select(year,
           starts_with("income"), 
           starts_with("contribution"), 
           starts_with("tax"))
  
  contributions
}