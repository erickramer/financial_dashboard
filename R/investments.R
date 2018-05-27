simulate_portfolios = function(con, 
                               pct_bond = 0.2,
                               n_sims = 100,
                               init_f01k = 0,
                               init_ira = 0,
                               init_taxable = 0){
  .sim = function(id){
    
    stock = function(x) (1 - pct_bond) * x
    bond = function(x) pct_bond * x
    
    prices = sample_prices(n_months)
    
    # initial portfolio shares
    
    init_f01k_stock = stock(init_f01k) / prices$stock_price[1]
    init_ira_stock = stock(init_ira) / prices$stock_price[1]
    init_taxable_stock = stock(init_taxable) / prices$stock_price[1]
    
    init_f01k_bond = bond(init_f01k) / prices$bond_price
    init_ira_bond = bond(init_ira) / prices$bond_price[1]
    init_taxable_bond = bond(init_taxable) / prices$bond_price[1]
    
    # simulate preformance
    
    res = mcon %>%
      bind_cols(prices) %>%
      mutate_at(vars(ends_with("mcon")), funs(stock, bond)) %>%
      mutate(f01k_stock_bought = f01k_mcon_stock / stock_price,
             ira_stock_bought = ira_mcon_stock / stock_price,
             taxable_stock_bought = taxable_mcon_stock / stock_price,
             f01k_bond_bought = f01k_mcon_bond / bond_price,
             ira_bond_bought = ira_mcon_bond / bond_price,
             taxable_bond_bought = taxable_mcon_bond / bond_price) %>%
      mutate(f01k_stock_total = cumsum(f01k_stock_bought) + init_f01k_stock,
             ira_stock_total = cumsum(ira_stock_bought) + init_ira_stock,
             taxable_stock_total = cumsum(taxable_stock_bought) + init_taxable_stock,
             f01k_bond_total = cumsum(f01k_bond_bought) + init_f01k_bond,
             ira_bond_total = cumsum(ira_bond_bought) + init_ira_bond,
             taxable_bond_total = cumsum(taxable_bond_bought), init_taxable_bond) %>%
      mutate(f01k_stock_value = f01k_stock_total * stock_price,
             ira_stock_value = ira_stock_total * stock_price,
             taxable_stock_value = taxable_stock_total * stock_price,
             f01k_bond_value = f01k_bond_total * bond_price,
             ira_bond_value = ira_bond_total * bond_price,
             taxable_bond_value = taxable_bond_total * bond_price) %>%
      mutate(total_value = f01k_stock_value + ira_stock_value + 
               taxable_stock_value + f01k_bond_value + ira_bond_value +
               taxable_bond_value) %>%
      mutate(id = id) %>%
      select(id, month, ends_with("value"))
             
  } 
  
  loginfo("Converting to monthly contributions")
  
  mcon = convert_to_monthly(con)
  n_months = nrow(mcon)
  
  loginfo("Entering similuation loop")
  
  sims = map(1:n_sims, .sim) %>%
    bind_rows()
}

convert_to_monthly = function(con){
  
  convert = function(x) rep(x/12, 12)
  
  mcon = con %>%
    mutate(f01k_mcon = map(f01k_total_contribution, convert)) %>%
    mutate(ira_mcon = map(ira_contribution, convert)) %>%
    mutate(taxable_mcon = map(taxable_contribution, convert)) %>%
    mutate(month = map(year, function(y) (y-1)*12 + 1:12)) %>%
    select(month, ends_with("mcon")) %>%
    unnest()
}


