# app.R
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)

# Global variables to store data (loaded once when app starts)
load_all_data <- function() {
  # Load boundary data once
  la_boundaries <- st_read("Local_Authority_Districts_December_2022_UK_BGC_V2_8602835163392188905", quiet = TRUE)
  la_boundaries$country <- substr(la_boundaries$LAD22CD, 1, 1)
  
  # Pre-load all fuel poverty data
  all_data <- list()
  for (year in 2012:2021) {
    if (year <= 2019) {
      filepath <- paste0("Final Data Cleaned/Sub_Reg_Data_", year, "_LILEE.csv")
    } else {
      filepath <- paste0("Consistent Area Codes/Sub_Reg_Data_", year, "_Updated.csv")
    }
    fuel_data <- read.csv(filepath)
    
    # Pre-join with boundaries
    joined_data <- la_boundaries %>%
      left_join(fuel_data, by = c("LAD22CD" = "Area.Codes"))
    
    all_data[[as.character(year)]] <- joined_data
  }
  
  return(all_data)
}

# UI
ui <- fluidPage(
  titlePanel("UK Fuel Poverty Map (2012-2021)"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 # Year selector
                 sliderInput("year", 
                             "Select Year:",
                             min = 2012,
                             max = 2021,
                             value = 2012,
                             step = 1,
                             sep = ""),
                 
                 # Color scheme selector
                 radioButtons("color_scheme",
                              "Color Scheme:",
                              choices = c("Blue-Red" = "br",
                                          "Viridis" = "viridis"),
                              selected = "br"),
                 
                 # Difference view toggle
                 checkboxInput("show_diff", 
                               "Show Change from Previous Year", 
                               value = FALSE),
                 
                 hr(),
                 
                 # Add numeric summary
                 verbatimTextOutput("summary")
    ),
    
    mainPanel(width = 9,
              # Single map output
              plotOutput("map", 
                         height = "700px",
                         # Add zoom functionality without plotly
                         dblclick = "map_dblclick",
                         brush = brushOpts(
                           id = "map_brush",
                           resetOnNew = TRUE
                         )
              )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load all data at startup
  all_data <- load_all_data()
  
  # Reactive values for zoom
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Reset zoom on double click
  observeEvent(input$map_dblclick, {
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
  # Create map
  output$map <- renderPlot({
    # Get current year's data
    data <- all_data[[as.character(input$year)]]
    
    if(input$show_diff && input$year > 2012) {
      # Get previous year's data for comparison
      prev_data <- all_data[[as.character(input$year - 1)]]
      data$proportion_diff <- data$proportion - prev_data$proportion
      
      p <- ggplot() +
        geom_sf(data = filter(data, country != "E"),
                fill = "grey90",
                color = "white",
                size = 0.25) +
        geom_sf(data = filter(data, country == "E"),
                aes(fill = proportion_diff),
                color = "white",
                size = 0.25) +
        scale_fill_gradient2(
          low = "blue",
          mid = "white",
          high = "red",
          midpoint = 0,
          name = "Change in %"
        )
    } else {
      # Regular view
      p <- ggplot() +
        geom_sf(data = filter(data, country != "E"),
                fill = "grey90",
                color = "white",
                size = 0.25) +
        geom_sf(data = filter(data, country == "E"),
                aes(fill = proportion),
                color = "white",
                size = 0.25) +
        {if(input$color_scheme == "br")
          scale_fill_gradient2(
            low = "blue",
            mid = "white",
            high = "red",
            midpoint = median(filter(data, country == "E")$proportion, na.rm = TRUE),
            name = "% in Fuel Poverty"
          )
          else
            scale_fill_viridis(name = "% in Fuel Poverty")}
    }
    
    p + theme_void() +
      labs(title = paste("Fuel Poverty in", input$year)) +
      coord_sf(xlim = ranges$x, ylim = ranges$y)
  })
  
  # Update ranges for zoom
  observeEvent(input$map_brush, {
    brush <- input$map_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
  })
  
  # Summary statistics
  output$summary <- renderText({
    data <- all_data[[as.character(input$year)]]
    eng_data <- filter(data, country == "E")
    
    sprintf("England Summary %d:\nMean: %.1f%%\nMedian: %.1f%%\nRange: %.1f%% to %.1f%%",
            input$year,
            mean(eng_data$proportion, na.rm = TRUE),
            median(eng_data$proportion, na.rm = TRUE),
            min(eng_data$proportion, na.rm = TRUE),
            max(eng_data$proportion, na.rm = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)