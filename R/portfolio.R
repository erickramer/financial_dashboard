portfolio = function(df,
                     standard_deduction = T, 
                     other_deductions = 0,
                     pct_bond = 0.2, 
                     n_sims = 10, 
                     init_f01k = 0, 
                     init_ira = 0, 
                     init_taxable =0){
  
 
  
  con = df %>%
    taxes(standard_deduction, other_deductions) %>%
    contributions()
  
  loginfo("simulating portfolios")
  
  portfolios = simulate_portfolios(con, 
                                   pct_bond,
                                   n_sims,
                                   init_f01k,
                                   init_ira,
                                   init_taxable)
}

