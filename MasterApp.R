# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(plotly)
library(ggplot2)

# UI Definition
ui <- fluidPage(
  titlePanel("England Fuel Poverty & Winter Mortality Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Year selector
      sliderInput("year", 
                  "Select Year:",
                  min = 2012,
                  max = 2021,
                  value = 2012,
                  sep = ""),
      
      # View selector
      radioButtons("view_mode", 
                   "Select View:",
                   choices = c("Fuel Poverty" = "fuel",
                               "Winter Mortality" = "mortality",
                               "Combined Analysis" = "bivariate")),
      
      # Conditional UI elements based on view_mode
      conditionalPanel(
        condition = "input.view_mode == 'fuel'",
        radioButtons("fuel_metric", 
                     "Display:",
                     choices = c("Total Numbers" = "total",
                                 "Percentage" = "percent")),
        checkboxInput("show_changes", 
                      "Show Year-on-Year Changes", 
                      value = FALSE)
      ),
      
      # Statistics panel
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      leafletOutput("map", height = "500px"),
      
      # Conditional panels for additional visualizations
      conditionalPanel(
        condition = "input.view_mode == 'mortality'",
        plotOutput("mortality_trend", height = "200px")
      ),
      
      conditionalPanel(
        condition = "input.view_mode == 'bivariate'",
        plotOutput("bivariate_legend", height = "200px", width = "200px")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data loading functions
  fuel_poverty_data <- reactive({
    year <- input$year
    file_path <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year)
    if (!file.exists(file_path)) return(NULL)
    read.csv(file_path)
  })
  
  mortality_data <- reactive({
    mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
    mortality_col <- paste0("Winter mortality index ", input$year)
    if(!mortality_col %in% names(mortality_df)) return(NULL)
    
    data.frame(
      Area_Codes = mortality_df$`Area code`,
      mortality = as.numeric(mortality_df[[mortality_col]])
    )
  })
  
  # Load shape data
  shape_data <- reactive({
    st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp", quiet = TRUE) %>%
      st_transform(4326)
  })
  
  # Initialize base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2, lat = 54, zoom = 6)
  })
  
  # Map update observer
  observe({
    req(shape_data())
    shapes <- shape_data()
    
    # Different logic based on view mode
    if (input$view_mode == "fuel") {
      data <- fuel_poverty_data()
      if (is.null(data)) return()
      
      mapped_data <- shapes %>%
        left_join(data, by = c("LAD22CD" = "Area.Codes"))
      
      value_col <- if(input$fuel_metric == "total") "fuel_poor" else "proportion"
      
    } else if (input$view_mode == "mortality") {
      data <- mortality_data()
      if (is.null(data)) return()
      
      mapped_data <- shapes %>%
        left_join(data, by = c("LAD22CD" = "Area_Codes"))
      
      value_col <- "mortality"
      
    } else {
      # Bivariate logic will be added later
      return()
    }
    
    # Color palette
    pal <- colorBin(
      "YlOrRd",
      domain = mapped_data[[value_col]],
      bins = 7,
      na.color = "#808080"
    )
    
    # Update map
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = mapped_data,
        fillColor = ~pal(get(value_col)),
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = mapped_data[[value_col]],
        title = if(input$view_mode == "fuel") 
          if(input$fuel_metric == "total") "Fuel Poor Households" else "Proportion (%)"
        else "Winter Mortality Index",
        opacity = 0.7
      )
  })
  
  # Basic summary statistics
  output$summary_stats <- renderText({
    if (input$view_mode == "fuel") {
      data <- fuel_poverty_data()
      if (is.null(data)) return("No data available")
      
      if (input$fuel_metric == "total") {
        paste("Total fuel poor households:", format(sum(data$fuel_poor), big.mark = ","))
      } else {
        paste("Average proportion:", round(mean(data$proportion), 2), "%")
      }
    } else if (input$view_mode == "mortality") {
      data <- mortality_data()
      if (is.null(data)) return("No data available")
      
      paste("Average mortality index:", round(mean(data$mortality, na.rm = TRUE), 2))
    } else {
      "Select a view mode to see statistics"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)