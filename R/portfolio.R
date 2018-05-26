simulate_portfolio = function(income, 
                              f01k_contribution,
                              f01k_match_pct,
                              ira_contribution){
  
  df = data_frame(year = seq_along(income),
                  income = income,
                  f01k_contribution = f01k_contribution,
                  f01k_match_pct = f01k_match_pct,
                  ira_contribution = ira_contribution)
  
  taxes = df %>%
    mutate(taxable_income = income - f01k_contribution - ira_contribution) %>%
    mutate(federal_tax = map_dbl(taxable_income, federal_taxes)) %>%
    mutate(state_tax = map_dbl(taxable_income, state_taxes)) %>%
    mutate(ss_tax = map_dbl(taxable_income, ss_taxes)) %>%
    mutate(medicare_tax = map_dbl(taxable_income, medicare_taxes)) %>%
    mutate(total_tax = federal_tax + state_tax + ss_tax + medicare_tax) %>%
    mutate(net_income = income - total_tax) %>%
    mutate(effective_tax_rate = 1 - net_income / income)
  
  contributions = taxes %>%
    select(year, net_income, f01k_contribution, f01k_match_pct, ira_contribution) %>%
    mutate(saving_goal = net_income / 2) %>%
    mutate(f01k_match = min(income*f01k_match_pct, f01k_contribution)) %>%
    mutate(f01k_total_contribution = f01k_match + f01k_contribution) %>%
    mutate(taxable_contributions = saving_goal - f01k_contribution - 
             f01k_match - ira_contribution)
  
  portfolios = simulate_portfolio(contributions)
  
  # summary stats
  
  # some figures...
  
  # IRA 
  
  # taxable
}

income = c(1.8e5, 1.9e5, 2e5, 2e5, 4e5, 4.1e5, 4.1e5, 4.2e5, 4.2e5, 4.2e5)
f01k_contribution = rep(36000, 10)
ira_contribution = rep(11000, 10)
f01k_match_pct = rep(0.03, 10)
