simulate_investments = function(f01k_con,
                                ira_con,
                                taxable_con,
                                f01k_init,
                                ira_init,
                                taxable_init, 
                                stock_pct = 0.8, 
                                n_sims = 100){
  
  STOCK_MEAN_ROR = 1.009384
  STOCK_SD_ROR = 0.04423438
  
  BOND_MEAN_ROR = 1.004807
  BOND_SD_ROR = 0.01129537
  
  n_months = length(f01k_con)
  
  loginfo("simulating investments")

  
  contributions = cbind(rep(f01k_con / 12, each = 12),
                        rep(ira_con / 12, each = 12),
                        rep(taxable_con / 12, each = 12))
  
  init = c(f01k_init, ira_init, taxable_init)
  
  
  stocks = simulate(contributions * stock_pct, 
                 init * stock_pct, 
                 STOCK_MEAN_ROR, 
                 STOCK_SD_ROR, 
                 n_sims) %>%
    map(as_data_frame) %>%
    bind_rows() %>%
    rename(stock_f01k = V1, 
           stock_ira = V2, 
           stock_taxable = V3, 
           month = V4, 
           sim = V5)
  
  bonds = simulate(contributions * (1 - stock_pct), 
                   init * (1 - stock_pct), 
                   BOND_MEAN_ROR, 
                   BOND_SD_ROR, 
                   n_sims) %>%
    map(as_data_frame) %>%
    bind_rows() %>%
    rename(bond_f01k = V1, 
           bond_ira = V2, 
           bond_taxable = V3) %>%
    select(-V4, -V5)
  
  portfolio = bind_cols(stocks, bonds) %>%
    mutate(total_value = stock_f01k + stock_ira + stock_taxable +
             bond_f01k + bond_ira + bond_taxable)
  
  portfolio
}



