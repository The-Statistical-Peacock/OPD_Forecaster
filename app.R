# Required Libraries
library(shiny)
library(forecast)  
library(duckdb)    
library(dplyr)
library(thematic)
library(plotly)
library(ggplot2)   
library(bslib)    

# Define UI for the application with bslib theme
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar and Main Panel Content with Tabs
  layout_sidebar(
    sidebar = sidebar(
      h4("Filters (Tab 1 Only)"),
      selectInput("hospital", "Select Hospital:", choices = NULL),
      selectInput("specialty", "Select Specialty:", choices = NULL),
      actionButton("predict", "Predict Next 12 Weeks", class = 'btn-success')
    ),
    
    main = tabsetPanel(
      # Tab 1: Existing Filtering Functionality
      tabPanel(
        title = "By Hospital and Specialty",
        layout_columns(
          col_widths = c(9, 3),  
          card(
            card_header("AutoARIMA Prediction for Outpatient Waiting List Numbers", style = "background-color: #18BC9C; color: white;"),
            plotlyOutput("forecastPlot")
          ),
          card(
            card_header("Forecasted Values", style = "background-color: #18BC9C; color: white;"),
            tableOutput("forecastTable")
          )
        )
      ),
      
      # Tab 2: Aggregate Forecast for All Hospitals and Specialties
      tabPanel(
        title = "All Hospitals and Specialties",
        layout_columns(
          col_widths = c(9, 3),  
          card(
            card_header("Prediction for All Hospitals and Specialties", style = "background-color: #18BC9C; color: white;"),
            plotlyOutput("allForecastPlot")
          ),
          card(
            card_header("Forecasted Aggregate Values", style = "background-color: #18BC9C; color: white;"),
            tableOutput("allForecastTable")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Connect to DuckDB
  con <- dbConnect(duckdb::duckdb(), dbdir = "NTPF_WL.duckdb")
  
  # Populate Hospital dropdown dynamically
  hospitals_query <- 'SELECT DISTINCT "hospital name" FROM OPD ORDER BY "hospital name" ASC'
  updateSelectInput(session, "hospital", choices = dbGetQuery(con, hospitals_query)$`hospital name`)
  
  # Specialty Dropdown Update
  observeEvent(input$hospital, {
    if (!is.null(input$hospital)) {
      safe_hospital_name <- gsub("'", "''", input$hospital)
      specialties_query <- paste0(
        'SELECT DISTINCT "Specialty" FROM OPD WHERE "hospital name" = \'', safe_hospital_name, '\' ORDER BY "Specialty" ASC'
      )
      updateSelectInput(session, "specialty", choices = dbGetQuery(con, specialties_query)$Specialty)
    }
  })
  
  # Filtered Data for Tab 1
  filtered_data <- eventReactive(input$predict, {
    safe_hospital_name <- gsub("'", "''", input$hospital)
    safe_specialty <- gsub("'", "''", input$specialty)
    query <- paste0(
      'SELECT "report_date", "Current" FROM OPD WHERE "hospital name" = \'', safe_hospital_name,
      '\' AND "Specialty" = \'', safe_specialty, '\' ORDER BY "report_date"'
    )
    dbGetQuery(con, query)
  })
  
  # Filtered Data for Tab 2
  all_data <- reactive({
    query <- 'SELECT "report_date", "Current" FROM OPD ORDER BY "report_date"'
    dbGetQuery(con, query)
  })
  
  # Tab 1: Forecast Plot
  output$forecastPlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) > 0) {
      ts_data <- ts(data$Current, frequency = 52)
      model <- auto.arima(ts_data)
      forecast_data <- forecast(model, h = 12)
      
      forecast_plot(data, forecast_data)
    }
  })
  
  # Tab 2: Forecast Plot
  output$allForecastPlot <- renderPlotly({
    data <- all_data() %>%
      group_by(report_date) %>%
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      ts_data <- ts(data$Total, frequency = 52)
      model <- auto.arima(ts_data)
      forecast_data <- forecast(model, h = 12)
      
      forecast_plot(data, forecast_data)
    }
  })
  
  # Helper Function for Plotting
  forecast_plot <- function(data, forecast_data) {
    actual_df <- data.frame(Date = as.Date(data$report_date), Total = round(data$Current))
    forecast_df <- data.frame(
      Date = seq(from = max(as.Date(data$report_date)) + 7, by = "week", length.out = 12),
      Forecast = round(forecast_data$mean),
      Lower_80 = round(forecast_data$lower[, 1]),
      Upper_80 = round(forecast_data$upper[, 1]),
      Lower_95 = round(forecast_data$lower[, 2]),
      Upper_95 = round(forecast_data$upper[, 2])
    )
    
    plot_ly() %>%
      add_trace(data = actual_df, x = ~Date, y = ~Total, type = 'scatter', mode = 'lines+markers') %>%
      add_trace(data = forecast_df, x = ~Date, y = ~Forecast, type = 'scatter', mode = 'lines+markers') %>%
      add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_80, ymax = ~Upper_80, fillcolor = 'rgba(40, 167, 69, 0.2)') %>%
      add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_95, ymax = ~Upper_95, fillcolor = 'rgba(40, 167, 69, 0.4)') %>%
      layout(title = "", xaxis = list(title = ""), yaxis = list(title = ""))
  }
  
  # Tab 1: Forecast Table
  output$forecastTable <- renderTable({
    forecast_table(filtered_data())
  })
  
  # Tab 2: Forecast Table
  output$allForecastTable <- renderTable({
    forecast_table(all_data() %>% group_by(report_date) %>% summarise(Total = sum(Current)))
  })
  
  # Helper Function for Forecast Table
  forecast_table <- function(data) {
    if (nrow(data) > 0) {
      ts_data <- ts(data$Total, frequency = 52)
      forecast_data <- forecast(ts_data, h = 12)
      data.frame(
        Week = seq(from = max(data$report_date) + 7, by = "week", length.out = 12),
        Forecast = round(forecast_data$mean)
      )
    }
  }
  
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

shinyApp(ui = ui, server = server)
