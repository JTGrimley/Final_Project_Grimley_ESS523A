library(shiny)
library(leaflet)
library(plotly)
library(dataRetrieval)
library(tidyverse)

# Function to fetch available sites with error handling
getSites <- function(stateCd = "NM", parameterCd = "00060") {
  tryCatch({
    sites <- whatNWISsites(stateCd = stateCd, parameterCd = parameterCd)
    
    # Validate the retrieved sites
    if (nrow(sites) == 0) {
      stop("No sites found for the specified state and parameter.")
    }
    
    # Ensure longitude and latitude are numeric
    sites$dec_long_va <- as.numeric(sites$dec_long_va)
    sites$dec_lat_va <- as.numeric(sites$dec_lat_va)
    
    # Remove sites with NA coordinates
    sites <- sites[!is.na(sites$dec_long_va) & !is.na(sites$dec_lat_va), ]
    
    return(sites)
  }, error = function(e) {
    shiny::showNotification(
      paste("Error fetching sites:", e$message), 
      type = "error"
    )
    return(NULL)
  })
}

# UI definition with improved layout and error handling
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),  # Modern Bootstrap theme
  
  titlePanel("USGS Streamflow Data Explorer for New Mexico"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Adjust sidebar width
      selectInput("site", 
                  "Choose a USGS Site", 
                  choices = NULL,
                  width = "100%"),
      uiOutput("dateRangeUI"),
      actionButton("loadData", 
                   "Load Data", 
                   class = "btn-primary", 
                   width = "100%"),
      div(style = "margin-top: 15px;"),
      downloadButton("downloadData", "Download Data", class = "btn-success", width = "100%"),
      
      # Additional site information
      htmlOutput("siteInfoDisplay")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("How to Use",
                 div(class = "p-3", # Add some padding
                     h3("USGS Streamflow Data Explorer Guide"),
                     tags$div(
                       tags$h4("Getting Started"),
                       tags$ol(
                         tags$li("Site Selection: Use the dropdown menu to choose a USGS monitoring site in New Mexico."),
                         tags$li("Map Interaction: Click on map markers to quickly select different sites."),
                         tags$li("Date Range: After selecting a site, a date range selector will appear with available historical data.")
                       ),
                       tags$h4("Exploring Data"),
                       tags$ol(
                         tags$li("Load Data: Click the 'Load Data' button to retrieve streamflow information for the selected site and date range."),
                         tags$li("Map Tab: Shows the locations of all monitoring sites in New Mexico."),
                         tags$li("Time Series Tab: Displays a detailed graph of streamflow over the selected period.",
                                 tags$ul(
                                   tags$li("Hover over the graph to see exact values"),
                                   tags$li("Use the Plotly toolbar to zoom, pan, or download the graph")
                                 )
                         ),
                         tags$li("Data Summary Tab: Provides statistical overview of the streamflow data.")
                       ),
                       tags$h4("Data Download"),
                       tags$ol(
                         tags$li("After loading data, use the 'Download Data' button to save the current dataset as a CSV file."),
                         tags$li("The filename includes the site number and date range for easy reference.")
                       )
                     ),
                     tags$div(
                       class = "alert alert-info mt-3",
                       tags$strong("Note:"),
                       "Data is sourced from USGS National Water Information System (NWIS). Streamflow is measured in cubic feet per second (cfs)."
                     )
                 )
        ),
        tabPanel("Map", 
                 leafletOutput("map", height = 500)),
        tabPanel("Time Series", 
                 plotlyOutput("timeSeries", height = 500)),
        tabPanel("Data Summary", 
                 verbatimTextOutput("summary"))
      )
    )
  )
)
# Server logic with improved error handling and features
server <- function(input, output, session) {
  
  # Reactive sites with error handling
  sites <- reactive({
    req(getSites())
  }) %>% 
    bindCache(Sys.Date())  # Cache sites to reduce unnecessary API calls
  
  # Update site dropdown
  observe({
    req(sites())
    site_choices <- setNames(sites()$site_no, sites()$station_nm)
    
    updateSelectInput(session, "site", 
                      choices = site_choices, 
                      selected = site_choices[1])
  })
  
  # Add this to your server function, after the observeEvent(input$loadData, {...})
  # Download handler for the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("streamflow_data_", 
             input$site, 
             "_", 
             format(input$dateRange[1], "%Y%m%d"), 
             "_", 
             format(input$dateRange[2], "%Y%m%d"), 
             ".csv")
    },
    content = function(file) {
      req(input$site, input$dateRange)
      
      tryCatch({
        data <- readNWISdv(
          input$site, 
          parameterCd = "00060", 
          startDate = input$dateRange[1], 
          endDate = input$dateRange[2]
        ) %>% 
          renameNWISColumns()
        
        # Write the data to the file
        write.csv(data, file, row.names = FALSE)
        
        # Optional: Show a success notification
        showNotification("Data downloaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Error downloading data:", e$message), 
          type = "error"
        )
      })
    }
  )
  
  # Site information display
  output$siteInfoDisplay <- renderUI({
    req(input$site)
    site_info <- sites()[sites()$site_no == input$site, ]
    
    HTML(sprintf(
      "<strong>Selected Site:</strong> %s<br>
       <strong>Latitude:</strong> %.4f<br>
       <strong>Longitude:</strong> %.4f<br>
       <strong>HUC Code:</strong> %s",
      site_info$station_nm,
      site_info$dec_lat_va,
      site_info$dec_long_va,
      site_info$huc_cd %||% "N/A"
    ))
  })
  
  # Dynamically update date range
  observeEvent(input$site, {
    req(input$site)
    
    tryCatch({
      site_data <- readNWISdv(
        siteNumbers = input$site, 
        parameterCd = "00060", 
        startDate = "1900-01-01", 
        endDate = Sys.Date()
      )
      
      if (nrow(site_data) > 0) {
        output$dateRangeUI <- renderUI({
          dateRangeInput(
            "dateRange", 
            "Select Date Range", 
            start = min(site_data$Date, na.rm = TRUE),
            end = max(site_data$Date, na.rm = TRUE),
            min = min(site_data$Date, na.rm = TRUE),
            max = max(site_data$Date, na.rm = TRUE)
          )
        })
      } else {
        showNotification(
          "No historical data available for this site.", 
          type = "warning"
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Error retrieving site data:", e$message), 
        type = "error"
      )
    })
  })
  
  # Data loading and visualization
  observeEvent(input$loadData, {
    req(input$site, input$dateRange)
    
    tryCatch({
      data <- readNWISdv(
        input$site, 
        parameterCd = "00060", 
        startDate = input$dateRange[1], 
        endDate = input$dateRange[2]
      ) %>% 
        renameNWISColumns()
      
      # Time Series Plot
      output$timeSeries <- renderPlotly({
        plot_ly(data, x = ~Date, y = ~Flow, type = 'scatter', mode = 'lines') %>%
          layout(
            title = "Streamflow Time Series",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Flow (cfs)")
          ) %>%
          config(displayModeBar = TRUE)  # Always show interactive plotly toolbar
      })
      
      # Summary Statistics
      output$summary <- renderPrint({
        summary_stats <- summary(data$Flow)
        print(summary_stats)
        
        # Additional statistics
        cat("\n\nAdditional Flow Statistics:\n")
        cat("Standard Deviation:", sd(data$Flow, na.rm = TRUE), "\n")
        cat("Coefficient of Variation:", 
            sd(data$Flow, na.rm = TRUE) / mean(data$Flow, na.rm = TRUE), "\n")
      })
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message), 
        type = "error"
      )
    })
  })
  
  # Interactive Map
  output$map <- renderLeaflet({
    req(sites())
    
    leaflet(sites()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~dec_long_va, 
        lat = ~dec_lat_va,
        popup = ~station_nm, 
        layerId = ~site_no,
        radius = 5,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.7
      ) %>%
      setView(
        lng = mean(sites()$dec_long_va, na.rm = TRUE),
        lat = mean(sites()$dec_lat_va, na.rm = TRUE),
        zoom = 7
      )
  })
  
  # Map interaction
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click)
    updateSelectInput(session, "site", selected = input$map_marker_click$id)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
