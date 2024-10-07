# app.R

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
  # Apply bslib theme
  theme = bs_theme(
    version = 5,                    # Using Bootstrap 5
    bootswatch = "flatly",            # Pre-built theme from Bootswatch          
  ),
  
  tags$style(HTML("
    .card-header {
      background-color: #18bc9c;
      color: white;
      font-weight: bold;
    }
    .sidebar {
      background-color: #f7f7f7; /* Secondary color */
      color: white;
    }
    .main-panel {
      background-color: #2c3e50; /* Primary color */
      color: white;
    }
  ")),
  
  # Sidebar content
  sidebar = sidebar(
    h4("Filters"),
    selectInput("hospital", "Select Hospital:", choices = NULL),
    selectInput("specialty", "Select Specialty:", choices = NULL),
    actionButton("predict", "Predict Next 12 Weeks")
  ),
  
  # Main panel content with cards and columns
  layout_columns(
    col_widths = c(9, 3),  
    
    # First column: Card for plot
    card(
      card_header("AutoARIMA Prediction for Outpaitent Waiting List Numbers"),
      plotlyOutput("forecastPlot")
    ),
    
    # Second column: Card for table
    card(
      card_header("Forecasted Values"),
      tableOutput("forecastTable")
    )
  )
)

# Modified server function with proper column name quoting
server <- function(input, output, session) {
  # Connect to DuckDB
  con <- dbConnect(duckdb::duckdb(), dbdir = "NTPF_WL.duckdb")
  
  # Populate Hospital dropdown dynamically and sort hospitals alphabetically
  hospitals_query <- 'SELECT DISTINCT "hospital name" FROM OPD ORDER BY "hospital name" ASC'
  updateSelectInput(session, "hospital", choices = dbGetQuery(con, hospitals_query)$`hospital name`)
  
  # Reactive expression to update Specialty based on selected hospital
  observeEvent(input$hospital, {
    if (!is.null(input$hospital)) {
      # Escape single quotes in the hospital name
      safe_hospital_name <- gsub("'", "''", input$hospital)
      
      # Query to get specialties related to the selected hospital
      specialties_query <- paste0(
        'SELECT DISTINCT "Specialty" FROM OPD WHERE "hospital name" = \'', safe_hospital_name, '\' ORDER BY "Specialty" ASC'
      )
      specialties <- dbGetQuery(con, specialties_query)$Specialty
      
      # Update the Specialty selectInput based on the query result
      updateSelectInput(session, "specialty", choices = specialties)
    }
  })
  
  # Reactive expression to fetch and filter the data based on user input
  filtered_data <- eventReactive(input$predict, {
    # Escape single quotes in the hospital and specialty names
    safe_hospital_name <- gsub("'", "''", input$hospital)
    safe_specialty <- gsub("'", "''", input$specialty)
    
    query <- paste0(
      'SELECT "report_date", "Current" FROM OPD WHERE "hospital name" = \'', safe_hospital_name,
      '\' AND "Specialty" = \'', safe_specialty, '\' ORDER BY "report_date"'
    )
    dbGetQuery(con, query)
  })
  
  

  # Perform AutoARIMA Forecast
  output$forecastPlot <- renderPlotly({
    data <- filtered_data() %>% 
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
      
      # Plot using your specified styling
      p <- plot_ly() %>%
        add_trace(data = actual_df, x = ~Date, y = ~Total, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#5F3DC4", size = 8, line = list(color = '#9BAAB3', width = 2)),
                  line = list(shape = 'spline', smoothing = 1.3, color = "#5F3DC4", width = 3),
                  name = "Actual") %>%
        add_trace(data = forecast_df, x = ~Date, y = ~Forecast, 
                  type = 'scatter', mode = 'lines+markers',
                  marker = list(color = "#FF7F0E", size = 8, line = list(color = '#9BAAB3', width = 2)),
                  line = list(shape = 'spline', smoothing = 1.3, color = "#FF7F0E", width = 3),
                  name = "Forecast") %>%
        add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_80, ymax = ~Upper_80, 
                    fillcolor = 'rgba(92, 184, 92, 0.1)', line = list(width = 0), name = "80% CI") %>%
        add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lower_95, ymax = ~Upper_95, 
                    fillcolor = 'rgba(92, 184, 92, 0.2)', line = list(width = 0), name = "95% CI") %>% 
        layout(title = "",
               xaxis = list(title = "", type = "date", tickangle = 45, tickfont = list(size = 12, color = 'white'), showgrid = FALSE, tickformat = '%d-%b-%y'),
               yaxis = list(title = "", tickfont = list(size = 12, color = 'white'), tickformat = ".0f", showgrid = FALSE),  # Change format to whole numbers
               paper_bgcolor = '#5A5A5A',  
               plot_bgcolor = '#5A5A5A',
               legend = list(font = list(color = 'white')), 
               font = list(size = 14),
               margin = list(b = 50)) %>%
        config(displayModeBar = FALSE)
      
      p
    }
  })
  
  
  
  
  
  # Display forecasted values in a table
  output$forecastTable <- renderTable({
    data <- filtered_data() %>% 
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
        Forecast = round(as.numeric(forecast_data$mean))  # Round to nearest whole number
      )
      
      return(forecast_table)
    }
  })
  
  
  # Disconnect the database connection when the app is stopped
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
