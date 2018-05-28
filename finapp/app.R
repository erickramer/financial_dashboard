#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(purrr)
library(rhandsontable)
library(readr)
library(logging)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

basicConfig()

source("../R/taxes.R")
source("../R/investments.R")
source("../R/utils.R")
source("../R/portfolio.R")

editTable <- function(DF, outdir=getwd(), outfilename="table"){
  ui <- shinyUI(fluidPage(
    
    titlePanel("Kranini Family Financial Planning"),
    fluidPage(
      br(),
      tabsetPanel(
        tabPanel("Parameters", 
                 br(),
                 h2("Presets"),
                 fluidRow(column(4,
                                 wellPanel(
                                   actionButton("preset_ds", "Data Science Careers"),
                                   br(),
                                   actionButton("preset_bcb", "Black Cat Bio"),
                                   br(),
                                   actionButton("preset_res", "DS for Page, Residency for Eric")))),
                 br(),
                 h2("Contributions"),
                 fluidRow(column(4, 
                                 numericInput("init_f01k",
                                              "Initial 401k", 
                                               2.4e4, 
                                              min = 0),
                                 numericInput("init_ira",
                                              "Initial IRA", 
                                              4e4, 
                                              min = 0),
                                 numericInput("init_taxable",
                                              "Initial Taxable",
                                              2.4e4,
                                              min = 0)), 
                          column(8, rHandsontableOutput("hot"))),
                 br(),
                 h2("Simulation parameters"),
                 fluidRow(column(4,
                                 sliderInput("n_sims", "Number of Simulations",
                                             min = 1, max = 300, value = 10)))
                 ),
        tabPanel("Income and Contributions",
                 br(),
                 dataTableOutput('income')),
        tabPanel("Taxes",
                 br(),
                 dataTableOutput('taxes')),
        tabPanel("FI",
                 br(),
                 fluidRow(column(4, 
                                 wellPanel(
                                 h3(textOutput("final_value_mean")),
                                 h3(textOutput("final_value_median")),
                                 h3(textOutput("final_value_10th")),
                                 h3(textOutput("final_value_90th")))),
                          column(8,
                                 plotOutput("plot_final_value")))),
        tabPanel("Portfolio Growth",
                 br(),
                 fluidRow(column(12,
                                 plotOutput("plot_growth_value"))),
                 br(),
                 fluidRow(column(12,
                                 plotOutput("plot_bucket_value")))),
        tabPanel("Retirement")
      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    p = reactiveVal()
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })
    
    observe({
      p(portfolio(values[["DF"]], 
                standard_deduction = T, 
                other_deductions = 0,
                pct_bond = 0.2, 
                n_sims = input$n_sims, 
                init_f01k = input$init_f01k, 
                init_ira = input$init_ira, 
                init_taxable = input$init_taxable))
    })
    
    observeEvent(input$preset_ds, {
      df = data.frame(year = 1:12,
                      page_income = c(1.3e5,
                                      1.35e5,
                                      1.4e5,
                                      1.45e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5),
                      eric_income = c(1.3e5,
                                      1.35e5,
                                      1.4e5,
                                      1.45e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5),
                      f01k_contribution = rep(36000, 12),
                      f01k_match_pct = rep(0.03, 12),
                      ira_contribution = rep(11000, 12),
                      savings_rate = rep(0.5, 12))
      
      values[["DF"]] = df
      
    })
    
    observeEvent(input$preset_res, {
      df = data.frame(year = 1:12,
                      page_income = c(1.25e5,
                                      1.3e5,
                                      1.35e5,
                                      1.4e5,
                                      1.45e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5),
                      eric_income = c(0.6e5,
                                      0.62e5,
                                      0.64e5,
                                      0.8e5,
                                      2.4e5,
                                      2.6e5,
                                      2.8e5,
                                      3e5,
                                      3e5,
                                      3e5,
                                      3e5,
                                      3e5),
                      f01k_contribution = rep(36000, 12),
                      f01k_match_pct = rep(0.03, 12),
                      ira_contribution = rep(11000, 12),
                      savings_rate = c(rep(0.4, 4), rep(0.5, 8)))
      
      values[["DF"]] = df
      
    })
    
    observeEvent(input$preset_res, {
      df = data.frame(year = 1:10,
                      page_income = c(1.25e5,
                                      1.3e5,
                                      1.35e5,
                                      1.4e5,
                                      1.45e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5,
                                      1.6e5),
                      eric_income = c(0.6e5,
                                      0.62e5,
                                      0.64e5,
                                      0.8e5,
                                      2.4e5,
                                      2.6e5,
                                      2.8e5,
                                      3e5,
                                      3e5,
                                      3e5,
                                      3e5,
                                      3e5),
                      f01k_contribution = rep(36000, 12),
                      f01k_match_pct = rep(0.03, 12),
                      ira_contribution = rep(11000, 12),
                      savings_rate = c(rep(0.4, 4), rep(0.5, 8)))
      
      values[["DF"]] = df
      
    })
    
    # encapsulation for the portfolio simulation

    .p_summary = function(){
      
      p() %>%
        filter(month == max(month)) %>%
        summarize(mean = mean(total_value),
                  median = median(total_value),
                  tenth_pct = quantile(total_value, probs=0.1),
                  ninetieth_pct = quantile(total_value, probs=0.9))
    }
    
    # investments
    
    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = T, stretchH = "all")
    })
    
    output$taxes = renderDataTable({
      taxes(values[["DF"]]) %>%
        select(contains("income"), contains("tax"))
    })
    
    output$income = renderDataTable({
      values[["DF"]] %>%
        taxes() %>%
        contributions()
    })
    
    output$plot_final_value = renderPlot({
      
      colors = brewer.pal(4, "Dark2")
      
      p() %>% 
        filter(month == max(month)) %>%
        ggplot(aes(total_value)) +
        geom_histogram(fill = colors[1]) +
        labs(title="Final Portfolio Value") +
        labs(x="Total Value", y="Count") +
        theme(text = element_text(size = 20))
    })
    
    output$plot_growth_value = renderPlot({
      colors = brewer.pal(4, "Dark2")
      
      p() %>%
        ggplot(aes(month, total_value, group = id)) +
        geom_line() +
        labs(title="Total Portfolio Value") +
        labs(x="Month", y="Total Value") +
        theme(text = element_text(size = 20)) + 
        scale_y_continuous(labels = dollar)
    })
    
    output$plot_bucket_value = renderPlot({
      buckets = p() %>%
        select(-total_value) %>%
        gather("bucket", "value", -id, -month) %>%
        separate(bucket, into = c("bucket", "type", "tmp"), sep = "_") %>%
        select(-tmp) %>%
        group_by(id, month, bucket) %>%
        summarize(value = sum(value)) %>%
        ungroup()
      
      
      bucket_mapping = c(f01k = "401k",
                         ira = "IRA",
                         taxable = "Taxable")
      
      medians = buckets %>%
        group_by(bucket, month) %>%
        mutate(value = median(value)) %>%
        ungroup() %>%
        mutate(bucket = bucket_mapping[bucket])
      
      buckets %>%
        mutate(bucket = bucket_mapping[bucket]) %>%
        ggplot(aes(month, value, group = id, color = bucket)) +
        geom_line(alpha = 1 / input$n_sims) +
        geom_line(data = medians) +
        facet_wrap(~bucket) +
        scale_color_brewer(palette = "Dark2") +
        scale_y_continuous(labels = dollar) +
        theme(text = element_text(size = 20)) 
    })
    
    output$final_value_mean = renderText({
      tmp = .p_summary()
      glue::glue("Mean: {dollar(tmp$mean[1])}")
    })
    
    output$final_value_median = renderText({
      tmp = .p_summary()
      glue::glue("Median: {dollar(tmp$median[1])}")
    })
    
    output$final_value_10th = renderText({
      tmp = .p_summary()
      glue::glue("10th percentile: {dollar(tmp$tenth_pct[1])}")
    })
    
    output$final_value_90th = renderText({
      tmp = .p_summary()
      glue::glue("90th percentile: {dollar(tmp$ninetieth_pct[1])}")
    })
    
    
  })
  
  runApp(list(ui=ui, server=server))
  return(invisible())
}

df = data.frame(year = 1:10,
                page_income = rep(1.2e5, 10),
                eric_income = rep(1.2e5, 10),
                f01k_contribution = rep(36000, 10),
                f01k_match_pct = rep(0.03, 10),
                ira_contribution = rep(11000, 10),
                savings_rate = rep(0.5, 10))


editTable(df)

