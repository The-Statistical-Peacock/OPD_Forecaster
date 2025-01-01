# Required Libraries
library(shiny)
library(forecast)
library(duckdb)
library(dplyr)
library(thematic)
library(plotly)
library(ggplot2)
library(bslib)
library(prophet)

# Define UI with NavbarPage for Multi-Page Application
ui <- navbarPage(
  title = "Time Series Predictions",
  theme = bs_theme(version = 5, bootswatch = "flatly"), # Flatly theme
  
  # Tab 1: AutoARIMA Results
  tabPanel(
    title = "AutoARIMA",
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        selectInput("hospital_arima", "Select Hospital:", choices = NULL),
        selectInput("specialty_arima", "Select Specialty:", choices = NULL),
        actionButton("predict_arima", "Predict Next 12 Weeks", class = 'btn-success')
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("forecastPlot_arima")),
          tabPanel("Forecast Table", tableOutput("forecastTable_arima"))
        )
      )
    )
  ),
  
  # Tab 2: Prophet Results
  tabPanel(
    title = "Prophet",
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        selectInput("hospital_prophet", "Select Hospital:", choices = NULL),
        selectInput("specialty_prophet", "Select Specialty:", choices = NULL),
        actionButton("predict_prophet", "Predict Next 12 Weeks", class = 'btn-success')
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("forecastPlot_prophet")),
          tabPanel("Forecast Table", tableOutput("forecastTable_prophet"))
        )
      )
    )
  )
)

# Server logic for AutoARIMA and Prophet Models
server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "NTPF_WL.duckdb")
  
  # Populate dropdowns for both tabs
  hospitals_query <- 'SELECT DISTINCT "hospital name" FROM OPD ORDER BY "hospital name" ASC'
  updateSelectInput(session, "hospital_arima", choices = dbGetQuery(con, hospitals_query)$`hospital name`)
  updateSelectInput(session, "hospital_prophet", choices = dbGetQuery(con, hospitals_query)$`hospital name`)
  
  observeEvent(input$hospital_arima, {
    if (!is.null(input$hospital_arima)) {
      safe_hospital <- gsub("'", "''", input$hospital_arima)
      query <- paste0('SELECT DISTINCT "Specialty" FROM OPD WHERE "hospital name" = \'', safe_hospital, '\' ORDER BY "Specialty" ASC')
      updateSelectInput(session, "specialty_arima", choices = dbGetQuery(con, query)$Specialty)
    }
  })
  
  observeEvent(input$hospital_prophet, {
    if (!is.null(input$hospital_prophet)) {
      safe_hospital <- gsub("'", "''", input$hospital_prophet)
      query <- paste0('SELECT DISTINCT "Specialty" FROM OPD WHERE "hospital name" = \'', safe_hospital, '\' ORDER BY "Specialty" ASC')
      updateSelectInput(session, "specialty_prophet", choices = dbGetQuery(con, query)$Specialty)
    }
  })
  
  # AutoARIMA Logic
  filtered_data_arima <- eventReactive(input$predict_arima, {
    safe_hospital <- gsub("'", "''", input$hospital_arima)
    safe_specialty <- gsub("'", "''", input$specialty_arima)
    query <- paste0(
      'SELECT "report_date", "Current" FROM OPD WHERE "hospital name" = \'', safe_hospital,
      '\' AND "Specialty" = \'', safe_specialty, '\' ORDER BY "report_date"'
    )
    dbGetQuery(con, query)
  })
  
  # AutoARIMA Plot
  output$forecastPlot_arima <- renderPlotly({
    data <- filtered_data_arima() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Convert data to time series
      ts_data <- ts(data$Total, frequency = 52)  # Weekly data
      
      # Apply AutoARIMA
      model <- auto.arima(ts_data)
      forecast_data <- forecast(model, h = 12)  # Forecast next 14 weeks
      
      # Create data for Plotly
      forecast_df <- data.frame(
        Date = seq(from = max(data$report_date) + 7, by = "week", length.out = 12),  # Generate future dates
        Forecast = round(as.numeric(forecast_data$mean)),
        Lower_80 = round(as.numeric(forecast_data$lower[, 1])),
        Upper_80 = round(as.numeric(forecast_data$upper[, 1])),
        Lower_95 = round(as.numeric(forecast_data$lower[, 2])),
        Upper_95 = round(as.numeric(forecast_data$upper[, 2]))
      )
      
      # Combine actual data for plotting and round values
      actual_df <- data.frame(
        Date = as.Date(data$report_date),
        Total = round(as.numeric(data$Total))
      )
      
      # Plot using Flatly styling
      p <- plot_ly() %>%
        add_trace(data = actual_df, x = ~Date, y = ~Total, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#007BFF", size = 8, line = list(color = '#CCCCCC', width = 2)),  # Flatly primary color
                  line = list(shape = 'spline', smoothing = 1.3, color = "#007BFF", width = 3),
                  name = "Actual") %>%
        add_trace(data = forecast_df, x = ~Date, y = ~Forecast, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#28A745", size = 8, line = list(color = '#CCCCCC', width = 2)),  # Flatly success color
                  line = list(shape = 'spline', smoothing = 1.3, color = "#28A745", width = 3),
                  name = "Forecast") %>%
        add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_80, ymax = ~Upper_80, 
                    fillcolor = 'rgba(40, 167, 69, 0.2)', line = list(width = 0), name = "80% CI") %>%
        add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_95, ymax = ~Upper_95, 
                    fillcolor = 'rgba(40, 167, 69, 0.4)', line = list(width = 0), name = "95% CI") %>% 
        layout(title = "",
               xaxis = list(title = "", type = "date", tickangle = 45, 
                            tickfont = list(size = 12, color = 'black'), 
                            showgrid = FALSE, 
                            tickformat = '%d-%b-%y'),
               yaxis = list(title = "", 
                            tickfont = list(size = 12, color = 'black'), 
                            tickformat = ".0f",
                            gridcolor = 'rgba(204, 204, 204, 0.2)',  # Light grey with transparency
                            griddash = 'dash',  # This controls the gridline style
                            gridwidth = 1,
                            showgrid = TRUE),
               paper_bgcolor = '#F8F9FA', 
               plot_bgcolor = '#F8F9FA', 
               legend = list(font = list(color = 'black')), 
               font = list(size = 14),
               margin = list(b = 50))  %>%
        config(displayModeBar = FALSE)
      
      p
    }
  })
  
  # AutoArima Table
  output$forecastTable_arima <- renderTable({
    data <- filtered_data_arima() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Convert data to time series
      ts_data <- ts(data$Total, frequency = 52)  # Weekly data
      
      # Apply AutoARIMA
      model <- auto.arima(ts_data)
      forecast_data <- forecast(model, h = 12)  # Forecast next 4 weeks (14 days)
      
      # Get the last available report date
      last_report_date <- as.Date(data$report_date[nrow(data)])
      
      # Generate actual dates for the next 14 days (for weekly forecast)
      forecast_dates <- seq(last_report_date + 7, by = "week", length.out = 12)
      
      # Format the forecast dates to match Plotly formatting (e.g., "Oct-24")
      formatted_dates <- format(forecast_dates, "%d-%b-%y")
      
      # Return forecasted values with actual dates
      forecast_table <- data.frame(
        Week = formatted_dates,
        Forecast = as.integer(round(as.numeric(forecast_data$mean)))  # Round to nearest whole number
      )
      
      return(forecast_table)
    }
  })
  
  # Prophet Logic
  filtered_data_prophet <- eventReactive(input$predict_prophet, {
    safe_hospital <- gsub("'", "''", input$hospital_prophet)
    safe_specialty <- gsub("'", "''", input$specialty_prophet)
    query <- paste0(
      'SELECT "report_date", "Current" FROM OPD WHERE "hospital name" = \'', safe_hospital,
      '\' AND "Specialty" = \'', safe_specialty, '\' ORDER BY "report_date"'
    )
    dbGetQuery(con, query)
  })
  
  output$forecastPlot_prophet <- renderPlotly({
    data <- filtered_data_prophet() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Convert data to time series and prepare Prophet data
      prophet_data <- data.frame(ds = as.Date(data$report_date), y = data$Total)
      model <- prophet(prophet_data, 
                       n.changepoints = 5,
                       daily.seasonality = TRUE,
                       weekly.seasonality = TRUE,
                       yearly.seasonality = TRUE)
      
      # Create future data frame and generate forecast
      future <- make_future_dataframe(model, periods = 12, freq = "week")
      forecast_data <- predict(model, future)
      
      # Filter forecast data to only include dates after the last actual date
      last_actual_date <- max(prophet_data$ds)
      forecast_df <- data.frame(
        Date = as.Date(forecast_data$ds),
        Forecast = round(as.numeric(forecast_data$yhat)),
        Lower_80 = round(as.numeric(forecast_data$yhat_lower)),  # Lower bound for confidence interval
        Upper_80 = round(as.numeric(forecast_data$yhat_upper))   # Upper bound for confidence interval
      )
      filtered_forecast_df <- forecast_df[forecast_df$Date > last_actual_date, ]
      
      # Combine last actual value with the first forecast value
      combined_data <- rbind(
        data.frame(Date = last_actual_date, Total = tail(data$Total, 1)),  # Last actual point
        data.frame(Date = filtered_forecast_df$Date, Total = filtered_forecast_df$Forecast)  # Forecasted values
      )
      
      # Create data for Plotly (actual + combined forecast)
      actual_df <- data.frame(
        Date = as.Date(data$report_date),
        Total = round(as.numeric(data$Total))
      )
      
      # Plot using Plotly, connecting the last actual point to the first predicted point
      p <- plot_ly() %>%
        add_trace(data = actual_df, x = ~Date, y = ~Total, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#007BFF", size = 8, line = list(color = '#CCCCCC', width = 2)),  # Flatly primary color
                  line = list(shape = 'spline', smoothing = 1.3, color = "#007BFF", width = 3),
                  name = "Actual") %>%
        add_trace(data = combined_data, x = ~Date, y = ~Total, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#28A745", size = 8, line = list(color = '#CCCCCC', width = 2)),  # Flatly success color
                  line = list(shape = 'spline', smoothing = 1.3, color = "#28A745", width = 3),
                  name = "Forecast") %>%
        add_ribbons(data = filtered_forecast_df, x = ~Date, ymin = ~Lower_80, ymax = ~Upper_80, 
                    fillcolor = 'rgba(40, 167, 69, 0.3)', line = list(width = 0), name = "80% CI") %>%
        layout(title = "",
               xaxis = list(title = "", type = "date", tickangle = 45, 
                            tickfont = list(size = 12, color = 'black'), 
                            showgrid = FALSE, 
                            tickformat = '%d-%b-%y'),
               yaxis = list(title = "", 
                            tickfont = list(size = 12, color = 'black'), 
                            tickformat = ".0f",
                            gridcolor = 'rgba(204, 204, 204, 0.2)',  # Light grey with transparency
                            griddash = 'dash',  # This controls the gridline style
                            gridwidth = 1,
                            showgrid = TRUE),
               paper_bgcolor = '#F8F9FA', 
               plot_bgcolor = '#F8F9FA', 
               legend = list(font = list(color = 'black')), 
               font = list(size = 14),
               margin = list(b = 50)) %>%
        config(displayModeBar = FALSE)
      
      p
    }
  })
  
  output$forecastTable_prophet <- renderTable({
    data <- filtered_data_prophet() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Prepare data for Prophet (ds = date, y = value)
      prophet_data <- data.frame(ds = as.Date(data$report_date), y = data$Total)
      
      # Fit Prophet model
      model <- prophet(prophet_data,
                       n.changepoints = 5,
                       daily.seasonality = TRUE,
                       weekly.seasonality = TRUE,
                       yearly.seasonality = TRUE)
      
      # Create future data frame for 12 weeks
      future <- make_future_dataframe(model, periods = 12, freq = "week")
      
      # Generate forecast
      forecast_data <- predict(model, future)
      
      # Filter forecast data to only include dates after the last actual date
      last_actual_date <- max(prophet_data$ds)
      filtered_forecast_data <- forecast_data[forecast_data$ds > last_actual_date, ]
      
      # Extract the newest 12 forecasted points
      forecast_table <- data.frame(
        Week = format(as.Date(filtered_forecast_data$ds), "%d-%b-%y"),  # Format the dates
        Forecast = as.integer(round(filtered_forecast_data$yhat))  # Round forecasted values to whole numbers
      )[1:12, ]  # Only keep the first 12 rows (newest 12 predicted points)
      
      return(forecast_table)
    }
  })
  
  # Disconnect database when app stops
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)