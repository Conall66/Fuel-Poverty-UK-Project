# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)

# UI Definition
ui <- fluidPage(
  titlePanel("England Winter Mortality Index Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Year selector updated to match fuel poverty years
      sliderInput("year", 
                  "Select Year:",
                  min = 2012,
                  max = 2021,
                  value = 2012,
                  sep = ""),
      
      # Add explanation text
      helpText("Winter Mortality Index for English Local Authorities.",
               "Gray areas indicate regions where data is not available."),
      
      # Warning message for missing data
      htmlOutput("warning_message"),
      
      # Debug output
      verbatimTextOutput("debug_info"),
      
      # Summary statistics output
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load mortality data
  mortality_data <- reactive({
    year_selected <- input$year
    
    mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
    
    # Match with the correct winter mortality column
    mortality_col <- paste0("Winter mortality index ", year_selected)
    
    # Check if the column exists
    if(!mortality_col %in% names(mortality_df)) {
      return(NULL)  # Return NULL if data for the year doesn't exist
    }
    
    mortality_for_year <- data.frame(
      Area_Codes = mortality_df$`Area code`,
      mortality = as.numeric(mortality_df[[mortality_col]])
    )
    
    return(mortality_for_year)
  })
  
  # Warning message output
  output$warning_message <- renderUI({
    if(is.null(mortality_data())) {
      div(
        style = "color: red; background-color: #ffe6e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        "No data available for", input$year
      )
    }
  })
  
  # Load shape data
  shape_data <- reactive({
    sf_data <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp", quiet = TRUE) %>%
      st_transform(4326)
    return(sf_data)
  })
  
  # Create initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2, lat = 54, zoom = 6)  # Adjusted view to better center on UK
  })
  
  # Update map
  observe({
    req(shape_data())
    
    mort_data <- mortality_data()
    shapes <- shape_data()
    
    # If no mortality data, show empty map with message
    if(is.null(mort_data)) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = shapes,
          fillColor = "#808080",  # Gray color for no data
          fillOpacity = 0.7,
          weight = 1,
          color = "#444444",
          label = ~LAD22NM
        )
      return()
    }
    
    # Join data
    mapped_data <- shapes %>%
      left_join(mort_data, by = c("LAD22CD" = "Area_Codes"))
    
    # Create color palette
    pal <- colorBin(
      palette = "YlOrRd",
      domain = mapped_data$mortality,
      bins = 7,
      na.color = "#808080"
    )
    
    # Create labels with more information
    labels <- lapply(seq_len(nrow(mapped_data)), function(i) {
      if(is.na(mapped_data$mortality[i])) {
        return(HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "No data available"
        )))
      } else {
        return(HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "Winter Mortality Index: ", round(mapped_data$mortality[i], 1)
        )))
      }
    })
    
    # Update map
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = mapped_data,
        fillColor = ~pal(mortality),
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-family" = "sans-serif",
            padding = "6px",
            "background-color" = "white",
            "border-color" = "rgba(0,0,0,0.5)",
            "border-radius" = "4px"
          ),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = mapped_data$mortality,
        title = "Winter Mortality Index",
        opacity = 0.7,
        labFormat = labelFormat(digits = 1)
      )
  })
  
  # Debug output
  output$debug_info <- renderPrint({
    mort_data <- mortality_data()
    shapes <- shape_data()
    
    cat("Debug Information:\n")
    if(is.null(mort_data)) {
      cat("No mortality data available for selected year\n")
    } else {
      cat("Number of areas in mortality data:", nrow(mort_data), "\n")
    }
    cat("Number of areas in shape data:", nrow(shapes), "\n")
    cat("\nFirst few mortality records:\n")
    if(!is.null(mort_data)) print(head(mort_data))
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    mort_data <- mortality_data()
    if(!is.null(mort_data) && nrow(mort_data) > 0) {
      avg_mortality <- mean(mort_data$mortality, na.rm = TRUE)
      max_mortality <- max(mort_data$mortality, na.rm = TRUE)
      min_mortality <- min(mort_data$mortality, na.rm = TRUE)
      
      paste(
        "Average winter mortality index:", round(avg_mortality, 2), "\n",
        "Maximum value:", round(max_mortality, 2), "\n",
        "Minimum value:", round(min_mortality, 2)
      )
    } else {
      "No data available for selected year"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)