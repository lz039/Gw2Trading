# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(htmltools)
library(tidyverse)
library(readr)
library(plotly)

dir <- getwd()
date <- "2022-04-01"

df <- read.csv(paste(dir, "/../data/processed/gw2-all-processed-2022-04-01", ".csv", sep = ""))
sells_lasso_aug <- read.csv(paste(dir, "/../data/processed/gw2-lasso-model-result-2022-04-01", ".csv", sep = ""))
sells_rf_aug <- read.csv(paste(dir, "/../data/processed/gw2-rf-model-result-2022-04-01", ".csv", sep = ""))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
      title = "GW2 Trading",
      titleWidth = 200
    ), 
    dashboardSidebar(
      sidebarMenu(
        fluidRow(
          column(2),
          column(10, h2("Global filters"))
        ),
        fluidRow(
          column(12,
                 selectInput("rarity_input", h3("Rarity"), 
                             choices = c("All", df$rarity), selected = NULL))
        ),
        fluidRow(
          column(12,
                 selectInput("type_input", h3("Type"), 
                             choices = c("All", df$type, selected = NULL))
        )
      )
    )
    ),
    dashboardBody(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Profit Dashboard",
          value = "page1",
          fluidRow(
            column(
              width = 7,
              h2("Top profit items")
            ),
            column(
              width = 5,
              h2("Mean profit distribution")
            )
          ),
          fluidRow(
            column(
              width = 7,
              dataTableOutput("table")
            ),
            column(
              width = 5,
              plotOutput("price_distribution"),
              sliderInput("profit_range", h3("Profit in silver"), 
                          min = 0, max = round(max(df$profit) * 100, 0), value = c(0, round(max(df$profit) * 100, 0)), step = 1)
            )
          )
        ),
        tabPanel(
          title = "Model Dashboard",
          value = "page2",
          fluidRow(
            column(
              width = 12,
              h2("Model predictions on profit")
            )
          ),
          fluidRow(
            column(
              width = 12,
              plotlyOutput("plot_model")
            )
          ),
          fluidRow(
            column(12,
                   sliderInput("prediction_range", h3("Prediction in gold"), 
                               min = 0, max = round(max(sells_lasso_aug$.pred), 0), value = c(0, round(max(sells_lasso_aug$.pred), 0)), step = 1)
            )
          )
        ),
        tabPanel(
          title = "Data exploration",
          value = "page3",
          fluidRow(
            column(
              width = 6,
              plotlyOutput("type_plot_details")
            ),
            column(
              width = 6,
              plotlyOutput("type_plot")
            )
          )
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Render and filter table
  table_data <- reactive({
    table_data <-  df %>% select(id, name, profit, rarity, type)
    if (input$rarity_input != "All"){
      table_data <- table_data %>% subset(rarity == input$rarity_input)
    }
    else
    {
      table_data <- table_data
    }
    
    if (input$type_input != "All"){
      table_data <- table_data %>% subset(type == input$type_input)
    }
    else
    {
      table_data <- table_data
    }
    
    table_data <- table_data %>% 
      subset((profit*100) >= input$profit_range[1] & (profit*100) <= input$profit_range[2])
    })
  
  output$table <- renderDataTable(table_data())
  
  # Render and filter plot
  plot <- reactive({
    plot_profit_data <-  df %>% 
      group_by(name) %>% 
      summarise(mean_profit = mean(profit),
                type = unique(type),
                rarity = unique(rarity)) %>% 
      arrange(desc(mean_profit))
    
    if (input$rarity_input != "All"){
      plot_profit_data <- plot_profit_data %>% subset(rarity == input$rarity_input)
    }
    else
    {
      plot_profit_data <- plot_profit_data
    }
    
    if (input$type_input != "All"){
      plot_profit_data <- plot_profit_data %>% subset(type == input$type_input)
    }
    else
    {
      plot_profit_data <- plot_profit_data
    }
    
    plot_profit_data <- plot_profit_data %>% 
      subset((mean_profit*100) >= input$profit_range[1] & (mean_profit*100) <= input$profit_range[2])
    
    plot_profit_data %>% 
      ggplot() +
      geom_bar(aes(x =  mean_profit, fill = type), stat="count") +
      scale_x_binned(limits = c(0, 0.4)) +
      labs(x = "Mean profit", y = "Count",
           title = "Item profit distribution", subtitle = "Items by profit, in gold")
  })
  
  output$price_distribution <- renderPlot(plot())
  
  # Plot for model
  plot_model <- reactive({
    sells_join_aug <- sells_lasso_aug %>% 
      select(id = id, lasso_pred = .pred, unit_price_gold_sells, rarity, type) %>% 
      left_join(sells_rf_aug %>% select(id = id, rf_pred = .pred), by = "id")
    
    if (input$rarity_input != "All"){
      sells_join_aug <- sells_join_aug %>% subset(rarity == input$rarity_input)
    }
    else
    {
      sells_join_aug <- sells_join_aug
    }
    
    if (input$type_input != "All"){
      sells_join_aug <- sells_join_aug %>% subset(type == input$type_input)
    }
    else
    {
      sells_join_aug <- sells_join_aug
    }
    
    sells_join_aug <- sells_join_aug %>% 
      subset((rf_pred) >= input$prediction_range[1] & (rf_pred) <= input$prediction_range[2]) %>% 
      subset((lasso_pred) >= input$prediction_range[1] & (lasso_pred) <= input$prediction_range[2])
      
    ggplotly(sells_join_aug %>%  
      ggplot() +
      geom_point(aes(x = rf_pred, y = unit_price_gold_sells), color = "#05541a") +
      geom_point(aes(x = lasso_pred, y = unit_price_gold_sells), color = "#040552") +
      geom_abline(col = "red", lty = 2) +
      labs(x = "Prediction", y = "Gold value",
           title = "Item price predictions", subtitle = "Using random forest (green) and lasso regressions (blue)",
           color = c("A", "B")))
  })
    
  output$plot_model <- renderPlotly(plot_model())
  
  # Type distribution
  types_plot <- reactive({ 
    df_types <- df %>% 
    distinct(type, name) %>%
    group_by(type) %>% 
    count(type)
   
    if (input$type_input != "All"){
      df_types <- df_types %>% subset(type == input$type_input)
    }
    else
    {
      df_types <- df_types
    }
  
  types_plot <- df_types %>% 
    ggplot() +
    geom_bar(aes(x = type, y = n, fill = type), stat='identity') +
    theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position="bottom") +
    labs(title = "Type distribution", subtitle = "of all types", fill = "Types",
         x = "Type", y = "Count", caption = paste("Data from", params$data_date))
  
   ggplotly(types_plot)
  })
  
  output$type_plot <- renderPlotly(types_plot())
  
  # Detailed type distribution
  types_plot_details <- reactive({ 
    df_item_types <- df %>% 
    filter(type %in% c("Armor", "Weapon")) %>% 
    distinct(type, item_type, item_weight_class, name) %>%
    group_by(type, item_type, item_weight_class) %>% 
    count(item_type)

    types_plot_details <- df_item_types %>% 
      ggplot() +
      geom_bar(aes(x = item_type, y = n, fill = item_weight_class), stat='identity') +
      #facet_grid(df_item_types$type) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1)) +
      labs(title = "Detailed item types", subtitle = "Split by armors and weapons", fill = "Item weight",
           x = "Item types", y = "Count", caption = paste("Data from", params$data_date))
    
    types_plot_details <- ggplotly(types_plot_details)
  })
  
  output$type_plot_details <- renderPlotly(types_plot_details())
  
  rm(df_item_types)
}

shinyApp(ui = ui, server = server)
