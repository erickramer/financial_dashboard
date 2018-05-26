simulate_portfolios = function(contributions, 
                             n_months, 
                             n_simulations = 100){
  
  sp500 = readRDS("data/sp500.Rds") %>%
    as.tbl()
  
  starts = sp500() %>%
    filter(date < max(date) - months(n_months)) %>%
    sample_n(n_sim) %>%
    select(simulation_start = date) %>%
    mutate(simulation_id = row_number())

  f01k = .run_simulations(starts, contributions$f01k_total_contribution) %>%
    select(simulation_id, month, f01k_value = total_value)
  
  ira = .run_simulations(starts, contributions$ira_contribution) %>%
    select(simulation_id, month, ira_value = total_value)
  
  taxable = .run_simulations(starts, contributions$taxable_contributions) %>%
    select(simulation_id, month, taxable_value = total_value)

  investments = inner_join(f01k, ira, by = c("simulation_id", "month")) %>%
    inner_join(taxable, by = c("simulation_id", "month")) %>%
    mutate(total = f01k_value + ira_value + taxable_value)
}

.run_simulations = function(starts, yearly_contributions){
  monthly_contributions = rep(yearly_contributions / 12, each = 12)
  
  starts %>%
    mutate(data = map(simulation_start, 
                      .run_simulation,
                      monthly_contributions)) %>%
    unnest()
}

.run_simulation = function(start, monthly_contributions){
  
  n_months = length(monthly_contributions)
  
  end = start + months(n_months)
  
  eq1 = sp500() %>%
    filter(date >= start, date < end) %>%
    arrange(date) %>%
    mutate(month = row_number()) %>%
    select(-date)
  
  eq2 = eq1 %>%
    mutate(contribution = monthly_contributions) %>%
    mutate(shares_from_contribution = contribution / price) %>%
    mutate(shares_from_dividend = NA, total_shares = NA)
  
  for(i in seq(1, n_months)){
    if(i == 1){
      eq2$shares_from_dividend[i] = 0
      eq2$total_shares[i] = eq2$shares_from_contribution[i]
    } else{
      
      eq2$shares_from_dividend[i] = eq2$dividend[i] * eq2$total_shares[i-1]
      
      eq2$total_shares[i] = eq2$total_shares[i-1] + 
        eq2$shares_from_dividend[i] + 
        eq2$shares_from_contribution[i]
    }
  }

  eq3 = eq2 %>%
    mutate(total_value = total_shares * price)
  
  eq3
}
