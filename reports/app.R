# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(highcharter)
library(DT)
library(htmltools)
library(tidyverse)
library(readr)
library(ggplot2)
library(gt)
library(gtable)
library(DT)

dir <- getwd()
date <- "2022-04-01"

df <- read.csv(paste(dir, "/../data/processed/gw2-all-processed-2022-04-01", ".csv", sep = ""))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
      title = "GW2 Trading",
      titleWidth = 200
    ), 
    dashboardSidebar(
      sidebarMenu(
        fluidRow(
          column(12,
                 selectInput("rarity_input", h3("Rarity"), 
                             choices = c("All", df$rarity), selected = NULL)),
        ),
        fluidRow(
          column(12,
                 selectInput("type_input", h3("Type"), 
                             choices = c("All", df$type, selected = NULL)),
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
              plotOutput("price_distribution")
            )
          )
        ),
          tabPanel(
            title = "Outliers Dashboard",
            value = "page2",
            fluidRow(
              column(
                width = 6,
                h2("Outliers on profit and sells")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("outliers")
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
    
    plot_profit_data %>% 
      ggplot() +
      geom_histogram(aes(x =  mean_profit, fill = type), stat="count") +
      scale_x_binned(limits = c(0, 0.4)) +
      labs(x = "Mean profit", y = "Count",
           title = "Item profit distribution", subtitle = "Items by profit, in gold")
  })
  
  output$price_distribution <- renderPlot(plot())

  # Outliers
  output$outliers <- renderPlot(df %>% 
    subset(profit < 2.5 & unit_price_gold_sells < 2.5) %>% 
    ggplot() +  
    geom_boxplot(aes(x = 'Sells', y = unit_price_gold_sells)) +
    geom_text(aes(x = 'Sells', y = median(unit_price_gold_sells), label = median(unit_price_gold_sells)), size = 3, vjust = -1) +
    geom_boxplot(aes(x = 'Profit', y = profit)) +
    geom_text(aes(x = 'Profit', y = median(profit), label = median(profit)), size = 3, vjust = -0.5) +
    labs(title = "Outliers on profit and sells", subtitle = "Limit at 2.5 gold profit and sell price", x = "", y = "Price in gold")
  )  
}

# Run the application 
shinyApp(ui = ui, server = server)
