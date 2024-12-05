# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(ggplot2)

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
               "Gray areas indicate regions where data is not available.",
               "Click on any region to see historical trends."),
      
      # Warning message for missing data
      htmlOutput("warning_message"),
      
      # Area specific time series plot
      plotOutput("time_series", height = "300px"),
      
      # Selected area name
      textOutput("selected_area"),
      
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
  
  # Store the full mortality dataset
  full_mortality_data <- reactive({
    mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
    return(mortality_df)
  })
  
  # Store the selected region
  selected_region <- reactiveVal(NULL)
  
  # Load mortality data for specific year
  mortality_data <- reactive({
    year_selected <- input$year
    mortality_df <- full_mortality_data()
    
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
      setView(lng = -2, lat = 54, zoom = 6)
  })
  
  # Update map with click events
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
          fillColor = "#808080",
          fillOpacity = 0.7,
          weight = 1,
          color = "#444444",
          label = ~LAD22NM,
          layerId = ~LAD22CD  # Add layerId for click events
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
    
    # Create labels
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
        layerId = ~LAD22CD,  # Add layerId for click events
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
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
  
  # Handle map clicks
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    selected_region(click$id)
  })
  
  # Display selected area name
  output$selected_area <- renderText({
    req(selected_region())
    area_data <- full_mortality_data()
    area_name <- area_data$`Area name`[area_data$`Area code` == selected_region()]
    paste("Selected Area:", area_name)
  })
  
  # Create time series plot
  output$time_series <- renderPlot({
    req(selected_region())
    
    # Get the data for the selected region
    mortality_df <- full_mortality_data()
    region_data <- mortality_df[mortality_df$`Area code` == selected_region(), ]
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Year = 2012:2020,
      WMI = NA,
      Lower = NA,
      Upper = NA
    )
    
    # Fill in the data
    for(year in 2012:2020) {
      wmi_col <- paste0("Winter mortality index ", year)
      lower_col <- paste0("Lower Confidence Limit ", year)
      upper_col <- paste0("Upper Confidence Limit ", year)
      
      plot_data$WMI[plot_data$Year == year] <- region_data[[wmi_col]]
      plot_data$Lower[plot_data$Year == year] <- region_data[[lower_col]]
      plot_data$Upper[plot_data$Year == year] <- region_data[[upper_col]]
    }
    
    # Create the plot
    ggplot(plot_data, aes(x = Year, y = WMI)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      labs(
        title = "Winter Mortality Index Over Time",
        y = "Winter Mortality Index",
        caption = "Gray band shows 95% confidence interval"
      ) +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 8)
      )
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