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
library(vegalite)
library(magrittr)

basicConfig()

source("R/body.R")
source("R/investments_via_c.R")
source("R/incomes.R")
source("R/taxes.R")
source("R/contributions.R")
source("R/fire.R")
source("R/figs.R")

sourceCpp("src/investments.cpp")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  dashboardPage(
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    body
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive tables
  
  tbl_income = reactiveVal()
  tbl_income_valid = reactiveVal()
   
  observe({
    if(!is.null(input$tbl_income)){

      df = hot_to_r(input$tbl_income)
      
      if(all(!is.na(df$income1)))
        if(all(!is.na(df$income2)))
          tbl_income_valid(df %>% mutate(year = row_number()))
      
      tbl_income(df)
    } else{
      tbl_income_valid(tbl_income_default)
      tbl_income(tbl_income_default)
    }
  })
  
  tbl_all = reactive({
    tbl_income_valid() %>%
      calc_pretax_contributions(input$savings_rate) %>%
      calc_taxes() %>%
      calc_posttax_contributions(input$savings_rate)
  })

  tbl_contributions = reactive({
    tbl_all() %>%
      select(year, starts_with("contribution"))
  })
  
  tbl_simulations = reactive({
    con = tbl_contributions()
    simulate_investments(con$contribution_401k,
                         con$contribution_ira,
                         con$contribution_taxable,
                         input$init_f01k,
                         input$init_ira,
                         input$init_taxable,
                         stock_pct = input$equity_allocation,
                         n_sims = input$n_sims)
  })
  
  low = function(x) quantile(x, 0.05)
  high = function(x) quantile(x, 0.95)
  
  stats = reactive({
    renaming = c(f01k = "401k",
                 ira = "IRA",
                 taxable = "Taxable",
                 bond = "Bond",
                 stock = "Stock")
    
    tbl_simulations() %>%
      group_by(month) %>%
      summarize_at(vars(starts_with("stock"), starts_with("bond")),
                   .funs = funs(median, high, low)) %>%
      gather(var, value, -month) %>%
      separate(var, into = c("instrument", "holding", "stat"), remove = F) %>%
      mutate(col_var = paste(holding, instrument, sep="_")) %>%
      mutate(instrument = renaming[instrument]) %>%
      mutate(holding = renaming[holding]) %>%
      mutate(year = month / 12)
  })
  
  medians = reactive({
    stats() %>%
      filter(stat == "median")
  })
  
  extremes = reactive({
    stats() %>%
      filter(stat != "median") %>%
      mutate(ranking = ifelse(stat == "low", year, 2*max(year)-year + 1)) %>%
      arrange(instrument, holding, ranking)
  })
  
  
  output$tbl_income = renderRHandsontable({
    df = tbl_income()
    rhandsontable(df)
  })
  
  output$tbl_taxes = renderRHandsontable({
    df = tbl_all() %>%
      select(Year = year, 
             `Gross Income` = income, 
             `Taxble Income` = income_taxable, 
             `Total Tax` = tax_total, 
             `Effective Tax Rate` = tax_effective_rate)
    rhandsontable(df, readOnly = T)
  })
  
  output$tbl_contributions = renderRHandsontable({
    df = tbl_all() %>%
      select(Year = year,
             `Gross Income` = income,
             `Net Income` = income_after_taxes,
             `Contribution to IRA` = contribution_ira,
             `Contribution to 401k` = contribution_401k,
             `Contribution to Taxable Investments` = contribution_taxable) 
    rhandsontable(df, readOnly = T)
  })
  

  output$mean_portfolio = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    amount = scales::dollar(mean(df$total_value))
    valueBox(amount, "Average Final Portfolio") 
  })
  
  output$min_portfolio = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    amount = quantile(df$total_value, probs = 0.1)
    valueBox(scales::dollar(amount), "Final Portfolio - 10th Percentile") 
  })
  
  output$max_portfolio = renderValueBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    amount = quantile(df$total_value, probs = 0.9)
    valueBox(scales::dollar(amount), "Final Portfolio - 90th Percentile") 
  })
  
  
  output$annual_withdrawal = renderInfoBox({
    df = tbl_simulations() %>% 
      filter(month == max(month))
    principal = median(df$total_value)
    amount = scales::dollar(0.04 * principal)
    valueBox(amount, "Yearly Income from Final Portfolio") 
  })
  
  output$pct_fired = renderValueBox({
    income = tbl_all()
    simulations = tbl_simulations()
    p = pct_fired(income, simulations)

    col = if(p >= 2/3) "green" else if(p >= 1/3) "yellow" else "red"
    
    title = paste("Chance of being financially independent in",
                  nrow(income),
                  "years")
    
    valueBox(scales::percent(p), title, color = col) 
  })
  
  
  output$plt_income = renderVegalite({
    df = tbl_all() %>%
      select(year, income, income_after_taxes, income_take_home) %>%
      gather(type, income, -year) %>%
      mutate(type = ifelse(type == "income", "Gross Income",
                    ifelse(type == "income_after_taxes", "Net Income after Taxes",
                        "Take Home Income")))
    
    vegalite() %>%
      add_data(df) %>%
      cell_size(400, 200) %>%
      encode_x("year", "ordinal") %>%
      encode_y("income", "quantitative") %>%
      encode_color("type", "nominal") %>%
      scale_x_linear(zero = F) %>%
      axis_x(title = "Year") %>%
      axis_y(title = "Amount (USD)") %>%
      mark_line()
  })
  
  output$plt_contributions = renderVegalite({
    df = tbl_contributions() %>%
      select(year, 
             contribution_401k, 
             contribution_ira, 
             contribution_taxable) %>%
      gather(type, contribution, -year) %>%
      mutate(type = ifelse(type == "contribution_401k", "401k Contribution (with Match)",
                    ifelse(type == "contribution_ira", "IRA Contribution",
                                  "Taxable Contribution")))
    
    vegalite() %>%
      add_data(df) %>%
      cell_size(400, 200) %>%
      encode_x("year", "ordinal") %>%
      encode_y("contribution", "quantitative") %>%
      encode_color("type", "nominal") %>%
      scale_x_linear(zero = F) %>%
      axis_x(title = "Year") %>%
      axis_y(title = "Amount (USD)") %>%
      mark_line()
  })
  
  output$plt_portfolio = renderPlot({
    portfolio_figure(tbl_simulations())
  })
  
  output$plt_holding = renderPlot({
    portfolio_figure_holding(extremes(), medians())
  })
  
  output$plt_instrument = renderPlot({
    portfolio_figure_instrument(extremes(), medians())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

