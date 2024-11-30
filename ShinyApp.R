# app.R
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)
library(viridis)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("UK Fuel Poverty Map (2012-2021)"),
  
  fluidRow(
    column(3,
           wellPanel(
             # Year selector
             sliderInput("year", 
                         "Select Year:",
                         min = 2012,
                         max = 2021,
                         value = 2012,
                         step = 1,
                         sep = "",
                         animate = TRUE),
             
             # Color scheme selector
             selectInput("color_scheme",
                         "Color Scheme:",
                         choices = c("Blue-Red" = "br",
                                     "Viridis" = "viridis",
                                     "Magma" = "magma")),
             
             checkboxInput("show_diff", 
                           "Show Year-on-Year Change", 
                           value = FALSE),
             
             # Download button
             downloadButton("downloadMap", "Download Current Map")
           )
    ),
    
    column(9,
           fluidRow(
             column(8, 
                    # Main map with zoom
                    plotlyOutput("map", height = "600px")
             ),
             column(4,
                    # Small multiples showing previous years
                    plotOutput("small_multiples", height = "600px")
             )
           ),
           fluidRow(
             column(12,
                    # Sparkline trend for selected region
                    plotOutput("trend", height = "100px")
             )
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Cache to store data for all years
  data_cache <- reactiveVal(NULL)
  
  # Initialize cache
  observe({
    if (is.null(data_cache())) {
      all_data <- list()
      for (year in 2012:2021) {
        # All files are in Final Data Cleaned folder
        filepath <- paste0("Final Data Cleaned/Sub_Reg_Data_", year, "_LILEE.csv")
        fuel_data <- read.csv(filepath)
        all_data[[as.character(year)]] <- fuel_data
      }
      data_cache(all_data)
    }
  })
  
  # Function to get data for a specific year
  getData <- function(year) {
    # Read boundaries
    la_boundaries <- st_read("Local_Authority_Districts_December_2022_UK_BGC_V2_-7833039764417853105.csv", quiet = TRUE)
    
    # Get fuel data from cache
    fuel_data <- data_cache()[[as.character(year)]]
    
    # Identify countries for different coloring
    la_boundaries$country <- substr(la_boundaries$LAD22CD, 1, 1)
    
    # Join data
    uk_boundaries_with_data <- la_boundaries %>%
      left_join(fuel_data, by = c("LAD22CD" = "Area.Codes"))
    
    return(uk_boundaries_with_data)
  }
  
  # Create main map
  output$map <- renderPlotly({
    data <- getData(input$year)
    
    # Base plot
    p <- ggplot() +
      # Non-England regions in light grey
      geom_sf(data = filter(data, country != "E"),
              fill = "grey90",
              color = "white",
              size = 0.25) +
      # England with fuel poverty data
      geom_sf(data = filter(data, country == "E"),
              aes(fill = proportion,
                  text = paste(LAD22NM, "\nFuel Poverty:", 
                               round(proportion, 1), "%")),
              color = "white",
              size = 0.25) +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = median(filter(data, country == "E")$proportion, 
                          na.rm = TRUE),
        name = "% in Fuel Poverty"
      ) +
      theme_void() +
      labs(title = paste("Fuel Poverty in", input$year))
    
    # Convert to plotly for zoom
    ggplotly(p, tooltip = "text") %>%
      layout(dragmode = "zoom")
  })
  
  # Create small multiples of previous years
  output$small_multiples <- renderPlot({
    current_year <- input$year
    years_to_show <- max(2012, current_year - 3):current_year
    
    # Create a grid of small maps
    par(mfrow = c(4, 1), mar = c(1,1,2,1))
    for (year in years_to_show) {
      data <- getData(year)
      plot(data["proportion"], main = year, key.pos = NULL)
    }
  })
  
  # Create trend line for selected region
  output$trend <- renderPlot({
    # Add trend visualization here
    # This will show the trend for a selected region over time
  })
}

# Run the app
shinyApp(ui = ui, server = server)