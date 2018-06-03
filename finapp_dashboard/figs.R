con = ds10 %>%
  taxes() %>%
  contributions(fc = 0, sr = 0.5) %>%
  mutate(date = as.character(year + 2019))

con2 = con %>% 
  select(year, contains("income")) %>%
  gather(type, income, -year)
  
income_graph = nPlot(income ~ year,
                     group = "type",
                     data = con2, 
                     type = "lineChart",
                     axisLabel)
income_graph


income_graph2 = mPlot(x = "date", 
                      y = c("income", "net_income"), 
                      data = con, 
                      type = "Line",
                      ymin = 0,
                      hideHover = F,
                      preUnits = "$")
income_graph2


library(vegalite)

dat <- jsonlite::fromJSON('[
      {"a": "A","b": 28}, {"a": "B","b": 55}, {"a": "C","b": 43},
                          {"a": "D","b": 91}, {"a": "E","b": 81}, {"a": "F","b": 53},
                          {"a": "G","b": 19}, {"a": "H","b": 87}, {"a": "I","b": 52}
                          ]')

vegalite() %>%
  cell_size(400, 200) %>%
  add_data(con2) %>%
  encode_x("year", "ordinal") %>%
  encode_y("income", "quantitative") %>%
  encode_color("type", "nominal") %>%
  scale_x_linear(zero = F) %>%
  axis_x(title = "Year") %>%
  axis_y(title = "Income") %>%
  mark_line()

sims = simulate_investments(con$f01k_total_contribution,
                            con$ira_contribution,
                            con$taxable_contribution,
                            0, 0, 0, 0.8, 100)

top = function(x) quantile(x, probs = 0.9)
bottom = function(x) quantile(x, probs = 0.1)

quantiles = sims %>%
  group_by(month) %>%
  summarize_at(vars(contains("stock"), contains("bond"), total_value),
              funs(median, top, bottom)) %>%
  ungroup() %>%
  select(month, contains("top"), contains("bottom"), contains("median")) %>%
  gather(var, value, -month) %>%
  separate(var, into = c("instrument", "bucket", "bracket"), sep = "_")

vegalite() %>%
  cell_size(400, 400) %>%

vegalite() %>%
  cell_size(400, 400) %>%
  add_data(quantiles %>% filter(bucket == "total")) %>%
  encode_x("month", "quantitative") %>%
  encode_y("total_value", "quantitative") %>%
  encode_detail("sim", "nominal") %>%
  encode_color("bracket") %>%
  axis_x(title = "Year") %>%
  axis_y(title = "Value (USD)") %>%
  mark_line()
