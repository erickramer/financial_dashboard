.sp500 = function(){
  
  sp500_eq = Quandl::Quandl("MULTPL/SP500_REAL_PRICE_MONTH") %>%
    rename(date = Date, price = Value) %>%
    mutate(year = year(date), month = month(date)) %>%
    select(-date)
  
  sp500_div = Quandl::Quandl("MULTPL/SP500_DIV_YIELD_MONTH") %>%
    rename(date = Date, dividend = Value) %>%
    mutate(dividend = dividend / (12*100)) %>% # convert to monthly dividend
    mutate(year = year(date), month = month(date)) %>%
    select(-date)
  
  sp500 = inner_join(sp500_eq, sp500_div, by = c("year", "month")) %>%
    mutate(date = glue::glue("{year}-{month}-01")) %>%
    mutate(date = ymd(date)) %>%
    select(date, price, dividend) %>%
    as.tbl()
}

sp500 = function(path = "data/sp500.Rds"){
  if(file.exists(path)){
    readRDS(path)
  } else{
    .sp500() %T>% saveRDS(path)
  }
}