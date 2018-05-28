#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(Rcpp)

source("../R/loans.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(column(4, wellPanel(sliderInput("stock_pct", "Percentage Stocks",
                                                         value = 0.8, min = 0, max = 1),
                                             numericInput("loan_amount",
                                                          label = "Loan Amount",
                                                          value = 4e4, min = 0),
                                             numericInput("loan_interest_rate", 
                                                          label = "Annual Loan Interest Rate",
                                                          value = 0.04),
                                             numericInput("loan_term", 
                                                          label = "Loan Tearm (Years)",
                                                          value = 10),
                                             numericInput("n_sims",
                                                          label = "Number of Simulations",
                                                          value = 100, min = 0))),
                         column(8,valueBoxOutput("monthly_payment"),
                                  valueBoxOutput("median_portfolio"),
                                  valueBoxOutput("failure_rate"),
                                  plotOutput("hist"))))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  sim = reactive({
    
    simulate(input$loan_amount, 
             input$loan_interest_rate,
             input$loan_term*12,
             input$stock_pct,
             input$n_sims)
  })
  
  ends = reactive({
    sim() %>%
      filter(month == 120)
  })
  
  n_fails = reactive({
    n_fails = sim() %>%
      filter(month == 120) %>%
      summarize(counts = sum(principal < 0))
  })
  
  output$monthly_payment = renderValueBox({
    m = a(input$loan_amount, input$loan_interest_rate / 12, input$loan_term * 12)
    valueBox(dollar(m), "Monthly Payment", width = NULL, color = "teal")
  })

  output$median_portfolio = renderValueBox({
    m = dollar(floor(median(ends()$principal)))
    valueBox(m, "Median Final Portfolio")
  })
  
  output$failure_rate = renderValueBox({
    p = percent(n_fails()$counts[1] / input$n_sims)
    valueBox(p, "Portfolio Failure Rate")
  })
  
  output$hist = renderPlot({
    hist(ends()$principal, 
         breaks = 30, 
         col = "grey",
         xlab = "Final Portfolio Value",
         main = "Distribution of Final Portfolio Values",
         cex = 1.5)
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)


