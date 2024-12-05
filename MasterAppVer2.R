library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(plotly)
library(ggplot2)

# Define icons
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

ui <- fluidPage(
  titlePanel("England Fuel Poverty & Winter Mortality Dashboard"),
  
  fluidRow(
    column(3,
           wellPanel(
             sliderInput("year", "Select Year:", 2012, 2021, 2012, sep = ""),
             radioButtons("view_mode", "Select View:",
                          c("Fuel Poverty" = "fuel",
                            "Winter Mortality" = "mortality",
                            "Combined Analysis" = "bivariate")),
             conditionalPanel(
               condition = "input.view_mode == 'fuel'",
               radioButtons("fuel_metric", "Display:",
                            c("Total Numbers" = "total",
                              "Percentage" = "percent")),
               checkboxInput("show_changes", "Show Year-on-Year Changes", FALSE)
             ),
             htmlOutput("error_message")
           ),
           
           wellPanel(
             h4("Summary Statistics"),
             verbatimTextOutput("summary_stats")
           ),
           
           conditionalPanel(
             condition = "input.view_mode == 'mortality'",
             wellPanel(
               textOutput("selected_area"),
               plotOutput("mortality_trend", height = "200px")
             )
           )
    ),
    
    column(9,
           leafletOutput("map", height = "700px"),
           conditionalPanel(
             condition = "input.view_mode == 'bivariate'",
             absolutePanel(
               bottom = 10, right = 10,
               plotOutput("bivariate_legend", height = "150px", width = "150px")
             )
           )
    )
  )
)

server <- function(input, output, session) {
  selected_region <- reactiveVal(NULL)
  
  safe_read_csv <- function(file_path, ...) {
    tryCatch(read.csv(file_path, ...), error = function(e) NULL)
  }
  
  fuel_poverty_data <- reactive({
    file_path <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", input$year)
    safe_read_csv(file_path)
  })
  
  full_mortality_data <- reactive({
    safe_read_csv("WMI_RelevantYears.csv", check.names = FALSE)
  })
  
  mortality_data <- reactive({
    mortality_df <- full_mortality_data()
    if(is.null(mortality_df)) return(NULL)
    
    col <- paste0("Winter mortality index ", input$year)
    if(!col %in% names(mortality_df)) return(NULL)
    
    data.frame(
      Area_Codes = mortality_df$`Area code`,
      Area_Names = mortality_df$`Area name`,
      mortality = as.numeric(mortality_df[[col]])
    )
  })
  
  changes_data <- reactive({
    safe_read_csv("fuel_poverty_changes.csv")
  })
  
  shape_data <- reactive({
    tryCatch(
      st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp", quiet = TRUE) %>%
        st_transform(4326),
      error = function(e) NULL
    )
  })
  
  output$error_message <- renderUI({
    if(is.null(shape_data())) {
      return(div(class = "alert alert-danger", "Error: Unable to load map data"))
    }
    if(input$view_mode == "fuel" && is.null(fuel_poverty_data())) {
      return(div(class = "alert alert-warning", 
                 "No fuel poverty data available for selected year"))
    }
    if(input$view_mode == "mortality" && is.null(mortality_data())) {
      return(div(class = "alert alert-warning", 
                 "No mortality data available for selected year"))
    }
    NULL
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-2, 54, 6)
  })
  
  observe({
    req(shape_data())
    shapes <- shape_data()
    
    if(input$view_mode == "fuel") {
      data <- fuel_poverty_data()
      if(is.null(data)) return()
      
      mapped_data <- shapes %>%
        left_join(data, by = c("LAD22CD" = "Area.Codes"))
      
      value_col <- if(input$fuel_metric == "total") "fuel_poor" else "proportion"
      
      if(input$show_changes) {
        centroids <- st_centroid(shapes)
        centroids_df <- data.frame(
          Area_Codes = shapes$LAD22CD,
          Longitude = st_coordinates(centroids)[,1],
          Latitude = st_coordinates(centroids)[,2]
        )
      }
      
    } else if(input$view_mode == "mortality") {
      data <- mortality_data()
      if(is.null(data)) return()
      
      mapped_data <- shapes %>%
        left_join(data, by = c("LAD22CD" = "Area_Codes"))
      
      value_col <- "mortality"
    } else {
      return()
    }
    
    if(all(is.na(mapped_data[[value_col]]))) {
      mapped_data[[value_col]] <- 0  # Set default value
    }
    
    pal <- colorBin(
      "YlOrRd", 
      domain = if(!is.null(mapped_data[[value_col]])) {
        na.omit(mapped_data[[value_col]])
      } else {
        c(0, 1)  # default domain if data is NULL
      },
      bins = 7,
      na.color = "#808080"
    )
    
    map_proxy <- leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%  # clears markers upon toggle
      clearControls() %>%
      addPolygons(
        data = mapped_data,
        fillColor = ~pal(get(value_col)),
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        layerId = ~LAD22CD,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~sprintf(
          "<div style='font-family: sans-serif; padding: 8px;'><strong>%s</strong><br/>%s: %s</div>",
          LAD22NM,
          if(input$view_mode == "fuel") "Fuel Poor Households" else "Winter Mortality Index",
          if(input$view_mode == "fuel" && input$fuel_metric == "total") 
            format(fuel_poor, big.mark=",") 
          else round(get(value_col), 2)
        ) %>% lapply(HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = mapped_data[[value_col]],
        title = if(input$view_mode == "fuel") 
          if(input$fuel_metric == "total") "Fuel Poor Households" 
        else "Proportion (%)"
        else "Winter Mortality Index",
        opacity = 0.7
      )
    
    if(input$view_mode == "fuel" && input$show_changes) {
      changes <- changes_data() %>%
        filter(Year == input$year)
      
      if(!is.null(changes) && nrow(changes) > 0) {
        changes_with_loc <- changes %>%
          left_join(centroids_df, by = "Area_Codes") %>%
          mutate(
            icon_type = case_when(
              Percent_Change > 35 ~ "large_up",
              Percent_Change > 25 ~ "medium_up",
              Percent_Change > 20 ~ "small_up",
              Percent_Change < -35 ~ "large_down",
              Percent_Change < -25 ~ "medium_down",
              Percent_Change < -20 ~ "small_down",
              TRUE ~ NA_character_
            )
          ) %>%
          filter(!is.na(icon_type))
        
        for(icon_type in unique(changes_with_loc$icon_type)) {
          subset_data <- changes_with_loc[changes_with_loc$icon_type == icon_type, ]
          if(nrow(subset_data) > 0) {
            map_proxy <- map_proxy %>%
              addMarkers(
                data = subset_data,
                lng = ~Longitude,
                lat = ~Latitude,
                icon = icons[[icon_type]],
                label = ~sprintf("Change: %+.1f%%", Percent_Change)
              )
          }
        }
      }
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$view_mode == "mortality") {
      selected_region(input$map_shape_click$id)
    }
  })
  
  output$selected_area <- renderText({
    req(selected_region(), mortality_data())
    area_data <- mortality_data()
    area_name <- area_data$Area_Names[area_data$Area_Codes == selected_region()]
    paste("Selected Area:", area_name)
  })
  
  output$mortality_trend <- renderPlot({
    req(selected_region(), full_mortality_data())
    
    mortality_df <- full_mortality_data()
    region_data <- mortality_df[mortality_df$`Area code` == selected_region(), ]
    
    plot_data <- data.frame(
      Year = 2012:2020,
      WMI = NA,
      Lower = NA,
      Upper = NA
    )
    
    for(year in 2012:2020) {
      wmi_col <- paste0("Winter mortality index ", year)
      lower_col <- paste0("Lower Confidence Limit ", year)
      upper_col <- paste0("Upper Confidence Limit ", year)
      
      plot_data$WMI[plot_data$Year == year] <- region_data[[wmi_col]]
      plot_data$Lower[plot_data$Year == year] <- region_data[[lower_col]]
      plot_data$Upper[plot_data$Year == year] <- region_data[[upper_col]]
    }
    
    ggplot(plot_data, aes(x = Year, y = WMI)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray80", alpha = 0.5) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      labs(
        title = "Winter Mortality Index Over Time",
        y = "Winter Mortality Index",
        caption = "Gray band shows 95% confidence interval"
      )
  })
  
  output$summary_stats <- renderText({
    if(input$view_mode == "fuel") {
      data <- fuel_poverty_data()
      if(is.null(data)) return("No data available")
      
      if(input$fuel_metric == "total") {
        paste("Total fuel poor households:", 
              format(sum(data$fuel_poor), big.mark = ","))
      } else {
        paste("Average proportion:", 
              round(mean(data$proportion), 2), "%")
      }
    } else if(input$view_mode == "mortality") {
      data <- mortality_data()
      if(is.null(data)) return("No data available")
      
      paste("Average mortality index:", 
            round(mean(data$mortality, na.rm = TRUE), 2))
    } else {
      "Select a view mode to see statistics"
    }
  })
}

shinyApp(ui = ui, server = server)