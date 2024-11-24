# app.R

# Required Libraries
library(shiny)
library(prophet)
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
    version = 5,                   
    bootswatch = "flatly",                
  ),
  

  # Sidebar content
  sidebar = sidebar(
    h4("Filters"),
    selectInput("hospital", "Select Hospital:", choices = NULL),
    selectInput("specialty", "Select Specialty:", choices = NULL),
    actionButton("predict", "Predict Next 12 Weeks", class = 'btn-success')
  ),
  
  # Main panel content with cards and columns
  layout_columns(
    col_widths = c(9, 3),  
    
    # First column: Card for plot
    card(
      card_header("Prophet Predictions for Outpatient Waiting List Numbers", style = "background-color: #18BC9C; color: white;"),
      plotlyOutput("forecastPlot")
    ),
    
    # Second column: Card for table
    card(
      card_header("Forecasted Number of OPD Patients", style = "background-color: #18BC9C; color: white;"),
      tableOutput("forecastTable")
    )
  )
)


#------------------Server Function---------------------#
server <- function(input, output, session) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "NTPF_WL.duckdb")
  
  hospitals_query <- 'SELECT DISTINCT "hospital name" FROM OPD ORDER BY "hospital name" ASC'
  updateSelectInput(session, "hospital", choices = dbGetQuery(con, hospitals_query)$`hospital name`)
  
  observeEvent(input$hospital, {
    if (!is.null(input$hospital)) {
      safe_hospital_name <- gsub("'", "''", input$hospital)
      specialties_query <- paste0(
        'SELECT DISTINCT "Specialty" FROM OPD WHERE "hospital name" = \'', safe_hospital_name, '\' ORDER BY "Specialty" ASC'
      )
      specialties <- dbGetQuery(con, specialties_query)$Specialty
      updateSelectInput(session, "specialty", choices = specialties)
    }
  })
  
  filtered_data <- eventReactive(input$predict, {
    safe_hospital_name <- gsub("'", "''", input$hospital)
    safe_specialty <- gsub("'", "''", input$specialty)
    
    query <- paste0(
      'SELECT "report_date", "Current" FROM OPD WHERE "hospital name" = \'', safe_hospital_name,
      '\' AND "Specialty" = \'', safe_specialty, '\' ORDER BY "report_date"'
    )
    dbGetQuery(con, query)
  })
  
  # Perform Prophet Forecast and Update Plotly with Confidence Intervals
  output$forecastPlot <- renderPlotly({
    data <- filtered_data() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Convert data to time series and prepare Prophet data
      prophet_data <- data.frame(ds = as.Date(data$report_date), y = data$Total)
      model <- prophet(prophet_data)
      
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
  
  # Display only the newest 12 forecasted values in the table
  output$forecastTable <- renderTable({
    data <- filtered_data() %>% 
      group_by(report_date) %>% 
      summarise(Total = sum(Current))
    
    if (nrow(data) > 0) {
      # Prepare data for Prophet (ds = date, y = value)
      prophet_data <- data.frame(ds = as.Date(data$report_date), y = data$Total)
      
      # Fit Prophet model
      model <- prophet(prophet_data)
      
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
  
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
