parameters = tabItem(tabName = "parameters",
                     fluidRow(
                       column(6,
                         box(rHandsontableOutput("tbl_income"), 
                             title = "Income",
                             width = 12),
                         box(sliderInput("savings_rate", "Savings Rate", min = 0, max = 1,
                                         value = 0.5), width = 12)),
                       column(6,
                         box(numericInput("init_f01k", "Initial 401k", 12000, min = 0),
                             numericInput("init_ira", "Initial IRA", 30000, min = 0),
                             numericInput("init_taxable", "Initial Taxable", 12000, min = 0),
                             width = 12, title = "Initial Values for Accounts"),
                         box(numericInput("fk_pct", "401k Match", 0, min = 0, max = 0.06)),
                         box(numericInput("n_sims", "Number of Simulations", 100, min = 10)))
                     ))

taxes = tabItem(tabName = "taxes",
                fluidRow(
                box(rHandsontableOutput("tbl_taxes"), 
                    title = "Taxes",
                    width = 12)))

contributions = tabItem(tabName = "income",
                  fluidRow(
                    box(rHandsontableOutput("tbl_contributions"), 
                        title = "Contributions",
                        width = 12)))

summary = tabItem(tabName = "summary",
                  fluidRow(
                    valueBoxOutput("median_portfolio"),
                    valueBoxOutput("mean_portfolio"),
                    valueBoxOutput("annual_withdrawal")
                  ))

body = dashboardBody(
  summary,
  parameters,
  taxes,
  contributions
)