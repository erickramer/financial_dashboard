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
library(rhandsontable)
library(tidyverse)
library(Rcpp)
library(logging)

basicConfig()

source("sidebar.R")
source("body.R")
source("incomes.R")

source("../R/investments_via_c.R")
source("../R/taxes.R")

sourceCpp("../src/investments.cpp")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  dashboardPage(
    dashboardHeader(disable = T),
    sidebar,
    body
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive tables
  
  tbl_income_valid = reactiveVal()
   
  tbl_income = reactive({
    if(!is.null(input$tbl_income)){
      df = hot_to_r(input$tbl_income)
      
      if(all(!is.na(df$eric_income)))
        if(all(!is.na(df$page_income)))
          tbl_income_valid(df %>% mutate(year = row_number()))
      
      df
    } else{
      tbl_income_valid(tbl_income_default)
      tbl_income_default
    }
  })
  
  tbl_taxes = reactive({
    tbl_income_valid() %>%
      taxes()
  })

  tbl_contributions = reactive({
    tbl_taxes() %>%
      contributions(fc = input$fk_pct, sr = input$savings_rate)
  })
  
  tbl_simulations = reactive({
    con = tbl_contributions()
    simulate_investments(con$f01k_total_contribution,
                        con$ira_contribution,
                        con$taxable_contribution,
                        input$init_f01k,
                        input$init_ira,
                        input$init_taxable,
                        n_sims = input$n_sims)
  })
  
  observeEvent(input$ds10,{
    
  })
  
  observeEvent(input$ds12,{
    tbl_income(ds12)
  })
  
  observeEvent(input$doc10,{
    tbl_income(doc10)
  })
  
  observeEvent(input$doc12,{
    tbl_income(doc12)
  })
  
  
  output$tbl_income = renderRHandsontable({
    df = tbl_income()
    rhandsontable(df)
  })
  
  output$tbl_taxes = renderRHandsontable({
    df = tbl_taxes() %>%
      select(year, income, taxable_income, total_tax, effective_tax_rate)
    rhandsontable(df, readOnly = T)
  })
  
  output$tbl_contributions = renderRHandsontable({
    df = tbl_contributions()
    rhandsontable(df, readOnly = T)
  })
  
  output$median_portfolio = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    amount = scales::dollar(median(df$total_value))
    valueBox(amount, "Median Portfolio") 
  })
  
  output$mean_portfolio = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    amount = scales::dollar(mean(df$total_value))
    valueBox(amount, "Mean Portfolio") 
  })
  
  output$annual_withdrawal = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    principal = median(df$total_value)
    amount = scales::dollar(0.04 * principal)
    valueBox(amount, "Annual Withdrawl") 
  })
}

tbl_income_default = ds10

# Run the application 
shinyApp(ui = ui, server = server)

