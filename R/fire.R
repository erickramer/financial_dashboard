pct_fired = function(income, simulations, pct_withdrawal = 0.04){
  
  final_portfolios = simulations %>%
    filter(month == max(month)) 
  
  final_income = income$income_take_home %>% tail(1)
  
  sum(final_portfolios$total_value*pct_withdrawal > final_income) / 
    nrow(final_portfolios)
}