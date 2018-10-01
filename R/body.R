parameters = tabItem(tabName = "parameters",
                     fluidRow(
                       column(6,
                              box(column(6, rHandsontableOutput("tbl_income")),
                                  column(6, rHandsontableOutput("tbl_takehome_income")),
                                  title = "Expected Pre-tax Income",
                                  width = 12)),
                       column(6,
                              box(sliderInput("savings_rate", "Savings Rate", min = 0, max = 1,
                                              value = 0.4), 
                                  sliderInput("equity_allocation", "Percentage of Stocks in Portfolio",
                                              min = 0, max = 1, value = 0.8), width = 12),
                              box(numericInput("n_sims", "Number of Simulations", 100, min = 10)))
                     ))

portfolio = tabItem(tabName = "portfolio",
                    fluidRow(
                      tabBox(title = "Results from Simulations", width = 12,
                             tabPanel("Total Portfolio", plotOutput("plt_portfolio")),
                             tabPanel("Asset Classes", plotOutput("plt_holding")),
                             tabPanel("Accounts", plotOutput("plt_instrument"))))
                    )

taxes = tabItem(tabName = "taxes",
                fluidRow(
                box(rHandsontableOutput("tbl_taxes"), 
                    title = "Taxes",
                    width = 8)))

contributions = tabItem(tabName = "income",
                  fluidRow(
                    box(rHandsontableOutput("tbl_contributions"), 
                        title = "Contributions",
                        width = 8)))

summary = tabItem(tabName = "summary",
                  fluidRow(
                    valueBoxOutput("mean_portfolio"),
                    valueBoxOutput("annual_withdrawal"),
                    valueBoxOutput("pct_fired")
                  ))

charts = tabItem(tabName = "charts",
                 fluidRow(
                   box(vegaliteOutput("plt_income", height = 300), 
                       title = "Income", width = 6),
                   box(vegaliteOutput("plt_contributions", height = 300), 
                       title = "Contributions", width = 6)
                 ))

additional_parameters = tabItem(tabName = "additional_parameters",
                                fluidRow(
                                     box(numericInput("init_f01k", "Initial 401k", 0, min = 0),
                                         numericInput("init_ira", "Initial IRA", 0, min = 0),
                                         numericInput("init_taxable", "Initial Taxable", 0, min = 0),
                                         numericInput("fk_pct", "401k Match", 0, min = 0, max = 0.06, step = 0.01),
                                         title = "Additional Parameters")
                                ))

body = dashboardBody(
  summary,
  parameters,
  portfolio,
  charts,
  taxes,
  contributions,
  additional_parameters
)