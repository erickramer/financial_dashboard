prepare_sample_prices = function(){

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
  
  sampler = function(n_months){
    
    n = nrow(prices)
    m = n - n_months
    
    if(m < 1)
      stop("can't sample that many months")
    
    i = base::sample.int(m, 1)
    
    prices %>%
      filter(row_number() >= i, row_number() < (i+n_months)) %>%
      select(contains("price"))
  }

  sampler
}

sample_prices = prepare_sample_prices()
