# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(plotly)
library(RColorBrewer)
library(leaflet.extras)

# UI Definition
ui <- fluidPage(
  titlePanel("UK Fuel Poverty Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Year selector
      sliderInput("year", 
                  "Select Year:",
                  min = 2012,
                  max = 2021,
                  value = 2012,
                  sep = ""),
      
      radioButtons("display_mode", 
                   "Display Mode:",
                   choices = c("Fuel Poverty" = "fuel",
                               "Winter Mortality" = "mortality",
                               "Combined View" = "combined")),
      
      # View type selector
      radioButtons("view_type", 
                   "View Type:",
                   choices = c("Total Numbers" = "total",
                               "Percentage" = "percent")),
      
      # Arrow toggle
      checkboxInput("show_arrows", 
                    "Show Change Indicators", 
                    value = TRUE),
      
      # Summary statistics output
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      # Main map output
      leafletOutput("map", height = "600px"),
      
      # Info box for hovering over regions
      absolutePanel(
        id = "changes_box",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 10, right = 10,
        width = 200,
        style = "padding: 10px; background: white; border-radius: 5px;",
        h4("Year-on-Year Changes"),
        textOutput("fuel_poverty_change"),
        textOutput("mortality_change")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  ### fuel poverty data reactive
  fuel_poverty_data <- reactive({
    year_selected <- input$year
    file_path <- file.path("Final Data Cleaned", 
                           paste0("Sub_Reg_Data_", year_selected, "_LILEE.csv"))
    
    # Print file path for debugging
    print(paste("Attempting to read:", file_path))
    
    # Check if file exists
    if (!file.exists(file_path)) {
      stop(paste("File not found:", file_path))
    }
    
    read.csv(file_path)
  })
  
  ### load shapefile data
  shape_data <- reactive({
    # Print working directory for debugging
    print(paste("Working directory:", getwd()))
    
    # Read shapefile and transform to WGS84
    sf_data <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp") %>%
      st_transform(4326)  # Transform to WGS84
  })
  
  # Load pre-calculated changes
  changes_data <- reactive({
    read.csv("fuel_poverty_changes.csv")
  })
  
  ### Icons for year on year indicators
  icons <- list(
    small_up = makeIcon(
      iconUrl = "icons/small-up-arrow.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 10
    ),
    medium_up = makeIcon(
      iconUrl = "icons/medium-up-arrow.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    ),
    large_up = makeIcon(
      iconUrl = "icons/large-up-arrow.png",
      iconWidth = 40, iconHeight = 40,
      iconAnchorX = 20, iconAnchorY = 20
    ),
    small_down = makeIcon(
      iconUrl = "icons/small-down-arrow.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 10
    ),
    medium_down = makeIcon(
      iconUrl = "icons/medium-down-arrow.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    ),
    large_down = makeIcon(
      iconUrl = "icons/large-down-arrow.png",
      iconWidth = 40, iconHeight = 40,
      iconAnchorX = 20, iconAnchorY = 20
    )
  )
  
  # create base map from leaflet (OpenSourceMap)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.5, lat = 53, zoom = 6) %>%
      addPolygons(data = shape_data(),
                  fillColor = "lightgrey",
                  weight = 1,
                  color = "black")
  })
  
  # Observe block - interactive changes
  observe({
    req(fuel_poverty_data(), shape_data())
    
    # Get the data
    data <- fuel_poverty_data()
    shapes <- shape_data()
    
    # Get centroids once, outside of any conditionals
    centroids <- st_centroid(shapes)
    centroids_df <- data.frame(
      Area_Codes = shapes$LAD22CD,
      Longitude = st_coordinates(centroids)[,1],
      Latitude = st_coordinates(centroids)[,2]
    )
    
    # Join the data
    mapped_data <- shapes %>%
      left_join(data, by = c("LAD22CD" = "Area.Codes"))
    
    # Create color palette
    pal <- colorBin(
      "YlOrRd", 
      domain = if(input$view_type == "total") mapped_data$fuel_poor 
      else mapped_data$proportion,
      bins = 7,
      na.color = "#808080"
    )
    
    # Base map update
    map_proxy <- leafletProxy("map") %>%
      clearShapes() %>% #clears old view
      clearMarkers() %>%  # clears markers upon toggle
      addPolygons(
        data = mapped_data,
        fillColor = ~pal(if(input$view_type == "total") fuel_poor else proportion),
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~lapply(paste0(
          "<b>", LAD22NM, "</b><br/>",
          "Fuel Poor: ", formatC(fuel_poor, format="f", big.mark=",", digits=0), "<br/>",
          "Proportion: ", formatC(proportion, format="f", digits=1), "%"
        ), HTML),
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
        values = if(input$view_type == "total") mapped_data$fuel_poor 
        else mapped_data$proportion,
        title = if(input$view_type == "total") "Fuel Poor Households"
        else "Proportion (%)",
        opacity = 0.7
      )
    
    # Add arrows if toggle is on
    if (input$show_arrows) {
      current_changes <- changes_data() %>%
        filter(Year == input$year, Significant == TRUE)
      
      if(nrow(current_changes) > 0) {
        # Join changes with centroids and determine icon size
        arrow_data <- current_changes %>%
          left_join(centroids_df, by = "Area_Codes") %>%
          mutate(
            icon_type = case_when(
              Direction == "increase" & abs(Percent_Change) > 35 ~ "large_up",
              Direction == "increase" & abs(Percent_Change) > 25 ~ "medium_up",
              Direction == "increase" ~ "small_up",
              Direction == "decrease" & abs(Percent_Change) > 35 ~ "large_down",
              Direction == "decrease" & abs(Percent_Change) > 25 ~ "medium_down",
              TRUE ~ "small_down"
            )
          )
        
        # Add markers for each size category
        for(icon_type in unique(arrow_data$icon_type)) {
          subset_data <- arrow_data[arrow_data$icon_type == icon_type, ]
          if(nrow(subset_data) > 0) {
            map_proxy <- map_proxy %>%
              addMarkers(
                data = subset_data,
                lng = ~Longitude,
                lat = ~Latitude,
                icon = icons[[icon_type]],
                label = ~paste0(
                  ifelse(Direction == "increase", "Increase: ", "Decrease: "), 
                  round(abs(Percent_Change), 1), "%"
                )
              )
          }
        }
      }
    }
  })
  
  # Generate summary statistics
  output$summary_stats <- renderText({
    req(fuel_poverty_data()) # Ensure data is available
    
    data <- fuel_poverty_data()
    
    if(input$view_type == "total") {
      total_fuel_poor <- sum(data$fuel_poor)
      paste("Total fuel poor households:", 
            format(total_fuel_poor, big.mark = ","))
    } else {
      avg_proportion <- mean(data$proportion)
      paste("Average proportion of fuel poor households:", 
            round(avg_proportion, 2), "%")
    }
  })
  
  # Update hover information
  output$hover_text <- renderText({
    "Hover over a region to see details"
  })
}

# Run the app
shinyApp(ui = ui, server = server)