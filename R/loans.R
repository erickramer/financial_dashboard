if(file.exists("./src/loan.cpp")) sourceCpp("./src/loan.cpp") else
  sourceCpp("../src/loan.cpp")

# prepping data

bonds_path = if(file.exists("./data/VBMFX.csv")) "./data/VBMFX.csv" else
  "../data/VBMFX.csv"

stocks_path = if(file.exists("./data/VFINX.csv")) "./data/VFINX.csv" else
  "../data/VFINX.csv"

bonds = read_csv(bonds_path, 
                 col_types = cols(
                   Date = col_date(format = ""),
                   Open = col_double(),
                   High = col_double(),
                   Low = col_double(),
                   Close = col_double(),
                   `Adj Close` = col_double(),
                   Volume = col_integer()
                 )) %>%
  select(date = Date, bond_price = `Adj Close`)

stocks = read_csv(stocks_path, 
                  col_types = cols(
                    Date = col_date(format = ""),
                    Open = col_double(),
                    High = col_double(),
                    Low = col_double(),
                    Close = col_double(),
                    `Adj Close` = col_double(),
                    Volume = col_integer()
                  )) %>%
  select(date = Date, stock_price = `Adj Close`)

prices = inner_join(bonds, stocks, by = "date")

rates = prices %>%
  mutate(stock_ror = stock_price / lag(stock_price),
         bond_ror = bond_price / lag(bond_price)) %>%
  filter(!is.na(stock_ror))

n = nrow(rates)

stock_ror_mu = mean(rates$stock_ror)
stock_ror_sigma = sd(rates$stock_ror)

bond_ror_mu = mean(rates$bond_ror, na.rm=T)
bond_ror_sigma = sd(rates$bond_ror)


a = function(p, i, n){
  p * i * (1+i)^n / ((1+i)^n - 1)
}

simulate = function(loan_amount, annual_i, n_months, stock_pct, n_sims = 100){

  monthly_i = annual_i / 12
  
  monthly_payment = a(loan_amount, monthly_i, n_months)
  
  sim = function(sim_id){
  
    df = data.frame(month = 1:n_months,
                    stock_ror = rnorm(n_months, stock_ror_mu, stock_ror_sigma),
                    bond_ror = rnorm(n_months, bond_ror_mu, bond_ror_sigma)) %>%
      mutate(total_ror = stock_ror * stock_pct + bond_ror * (1 - stock_pct)) %>%
      mutate(principal = calculate_principal(total_ror, loan_amount, monthly_payment))
      #mutate(principal = NA)
      
    # for(i in seq(1, n_months)){
    #   if(i == 1){
    #     df$principal[i] = loan_amount
    #   } else{
    #     if(df$principal[i-1] <= 0){
    #       df$principal[i] = df$principal[i-1] - monthly_payment
    #     } else{
    #       df$principal[i] = df$principal[i-1] * df$total_ror[i] - monthly_payment
    #     }
    #   }
    # }
    
    df %>% mutate(sim_id = sim_id)
  }
  
  sims = map(1:n_sims, sim) %>%
    bind_rows()
  
  sims

}



