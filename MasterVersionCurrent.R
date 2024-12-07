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

# Create bivariate color matrix function
bivariate_colour_matrix <- function() {
  matrix(
    c("#D3D3D3", "#689E73",  # Low mortality
      "#6277A5", "#30585C"),  # High mortality
    nrow = 2, 
    byrow = TRUE
  )
}


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
             textOutput("view_explanation"),
             conditionalPanel(
               condition = "input.view_mode == 'fuel'",
               selectInput("fuel_metric", "Display:",
                           choices = c(
                             "Total Numbers" = "total",
                             "Percentage" = "percent"
                           ),
                           width = "200px"
               ),
               checkboxInput("show_changes", "Show Year-on-Year Changes", FALSE)
             ),
             htmlOutput("warning_message")
           ),
           
           wellPanel(
             h4("Summary Statistics"),
             verbatimTextOutput("summary_stats")
           ),
           
           # Bivariate legend in left column
           conditionalPanel(
             condition = "input.view_mode == 'bivariate'",
             wellPanel(
               plotOutput("bivariate_legend", height = "150px", width = "150px")
             ),
             # Add rankings panel here
             wellPanel(
               h4("Most Affected Regions (2012-2020)"),
                  style = "font-size: 16px; margin-bottom: 15px; font-weight: 600;"),
               div(
                  style = "background-color: white; border-radius: 8px; padding: 12px;",
               tableOutput("fixed_rankings")
             )
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
           # Temporal visualization below map
           conditionalPanel(
             condition = "input.view_mode == 'bivariate'",
             wellPanel(
               h4(textOutput("selected_region_title")),
               plotOutput("temporal_bivariate", height = "150px")
             )
           )
    )
  )
)

server <- function(input, output, session) {
  
  global_medians <- readRDS("global_medians.rds")
  # Add this temporarily to your server function to check the global medians
  print("Global medians:")
  print(paste("Mortality median:", global_medians$mortality_median))
  print(paste("Fuel poverty median:", global_medians$fuel_poverty_median))
  
  selected_region <- reactiveVal(NULL)
  
  ###  DATA LOADING FUNCTIONS
  
  safe_read_csv <- function(file_path, ...) {
    tryCatch(read.csv(file_path, ...), error = function(e) NULL)
  }
  
  ### FUEL POVERTY DATA LOADING
  
  fuel_poverty_data <- reactive({
    file_path <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", input$year)
    safe_read_csv(file_path)
  })
  
  ### MORTALITY DATA LOADING
  
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
  ### leaderboard ting innit
  output$fixed_rankings <- renderTable({
    top_regions <- readRDS("top_regions.rds")
    data.frame(
      Region = top_regions$Area_Name,
      `High Risk Years` = top_regions$High_High_Count,
      `Avg WMI` = round(top_regions$Avg_Mortality, 1),
      `Avg FP%` = paste0(round(top_regions$Avg_Fuel_Poverty, 1), "%")
    )
  }, 
  align = 'lccc',  # Left align region, center align numbers
  width = "100%",
  striped = TRUE,
  hover = TRUE,
  spacing = "m",
  class = "table table-hover")
  
  # UI remains the same
  wellPanel(
    h4("Most Affected Regions (2012-2020)", 
       style = "font-size: 16px; margin-bottom: 15px; font-weight: 600;"),
    div(
      style = "background-color: white; border-radius: 8px; padding: 12px;",
      tableOutput("fixed_rankings")
    )
  )
  
  #OUTPUT TEXT EXPLANATION
  output$view_explanation <- renderText({
    switch(input$view_mode,
           "fuel" = "Fuel Poverty describes households that cannot afford to keep their home adequately heated at a reasonable cost, factoring in their income and home energy-efficiency",
           "mortality" = "The measure of excess deaths occurring in winter months compared to non-winter months. Winter Mortality Index (WMI) indicator describes the ratio of excess deaths in the winter months compared with the average number of deaths in the summer months",
           "bivariate" = "Bivariate graph shows the relationship between fuel poverty rates and winter mortality across regions"
    )
  })
  
  ### BIVARIATE DATA LOADING
  get_bivariate_data <- reactive({
    year_selected <- input$year
    mortality_df <- full_mortality_data()
    if(is.null(mortality_df)) return(NULL)
    
    # Get fuel poverty data with explicit column selection
    fp_data <- fuel_poverty_data()
    if(is.null(fp_data)) return(NULL)
    
    mortality_col <- paste0("Winter mortality index ", year_selected)
    if(!mortality_col %in% names(mortality_df)) return(NULL)
    
    # Create clear data frame with explicit columns
    combined_data <- data.frame(
      Area_Code = mortality_df$`Area code`,
      Area_Name = mortality_df$`Area name`,
      Mortality = as.numeric(mortality_df[[mortality_col]]),
      Fuel_Poverty = fp_data$proportion[match(mortality_df$`Area code`, fp_data$Area.Codes)]
    )
    
    # Add debugging output
    print("Combined Data Check:")
    print(head(combined_data))
    
    # Verify fuel poverty values explicitly
    print("Fuel Poverty Data Check:")
    print(head(fp_data %>% select(Area.Codes, proportion)))
    
    combined_data <- combined_data[complete.cases(combined_data), ]
    
    # Use global medians for classification
    combined_data$Mortality_Class <- ifelse(combined_data$Mortality > global_medians$mortality_median, "HIGH", "LOW")
    combined_data$Fuel_Poverty_Class <- ifelse(combined_data$Fuel_Poverty > global_medians$fuel_poverty_median, "HIGH", "LOW")
    
    return(combined_data)
  })
  
  
  selected_bivariate_region <- reactiveVal(NULL)
  
  # First define a correct color lookup function
get_bivariate_color <- function(is_high_mort, is_high_fp) {
  if(is_high_mort && is_high_fp) return("#30585C")      # High/High
  if(is_high_mort && !is_high_fp) return("#689E73")     # High/Low
  if(!is_high_mort && is_high_fp) return("#6277A5")     # Low/High
  return("#D3D3D3")                                     # Low/Low
}
  
  # Function to get historical bivariate data for a region
  get_historical_bivariate <- reactive({
    req(selected_bivariate_region())
    region_id <- selected_bivariate_region()
    
    # Print for debugging
    print(paste("Selected region:", region_id))
    
    years <- 2012:2020
    historical_data <- data.frame(
      Year = years,
      Mortality = NA,
      Fuel_Poverty = NA
    )
    
    mortality_df <- full_mortality_data()
    
    for(year in years) {
      # Get fuel poverty data for year
      fp_file <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year)
      fp_data <- safe_read_csv(fp_file)
      
      if(!is.null(fp_data)) {
        mortality_col <- paste0("Winter mortality index ", year)
        
        if(mortality_col %in% names(mortality_df)) {
          # Print values for debugging
          mort_val <- mortality_df[[mortality_col]][mortality_df$`Area code` == region_id]
          fp_val <- fp_data$proportion[fp_data$Area.Codes == region_id]
          print(paste("Year:", year, "Mortality:", mort_val, "FP:", fp_val))
          
          historical_data$Mortality[historical_data$Year == year] <- mort_val
          historical_data$Fuel_Poverty[historical_data$Year == year] <- fp_val
        }
      }
    }
    
    # Print results
    print("Final historical data:")
    print(historical_data)
    
    return(historical_data[complete.cases(historical_data), ])
  })
  
  # Improved bivariate legend using ggplot2
  output$bivariate_legend <- renderPlot({
    # Create a data frame for the legend
    legend_data <- expand.grid(
      x = 1:2,
      y = 1:2
    )
    
    # Add colors
    legend_data$fill <- as.vector(bivariate_colour_matrix())
    
    # Create the legend plot
    ggplot(legend_data, aes(x = x, y = y)) +
      geom_tile(aes(fill = fill)) +
      scale_fill_identity() +
      coord_fixed() +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title.x = element_text(hjust = 0.5, size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(hjust = 0.5, size = 10, angle = 90, margin = margin(r = 10)),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid = element_blank()
      ) +
      labs(
        title = "Bivariate Legend",
        x = "Fuel Poverty →",
        y = "Winter Mortality →"
      )
  })
  
  output$warning_message <- renderUI({
    year_selected <- input$year
    
    if(year_selected == 2021) {
      div(
        style = "color: red; background-color: #ffe6e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        "No winter mortality data available for 2021"
      )
    } else if(is.null(get_bivariate_data())) {
      div(
        style = "color: red; background-color: #ffe6e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        "No data available for", year_selected
      )
    }
  })
  
  # Output for region title
  output$selected_region_title <- renderText({
    req(selected_bivariate_region())
    region_name <- shape_data()$LAD22NM[shape_data()$LAD22CD == selected_bivariate_region()]
    paste("Historical Bivariate Pattern for", region_name)
  })
  
  # Output for temporal small multiples
  output$temporal_bivariate <- renderPlot({
    req(selected_bivariate_region())
    hist_data <- get_historical_bivariate()
    
    # Create a function to get color based on values directly
    get_color <- function(mortality, fuel_poverty) {
      is_high_mort <- mortality > global_medians$mortality_median
      is_high_fp <- fuel_poverty > global_medians$fuel_poverty_median
      
      # Use the same logic as the map
      if(is_high_mort && is_high_fp) return("#30585C")      # High/High
      if(is_high_mort && !is_high_fp) return("#689E73")     # High/Low
      if(!is_high_mort && is_high_fp) return("#6277A5")     # Low/High
      return("#D3D3D3")                                     # Low/Low
    }
    
    # Add colors directly based on values
    hist_data$fill_color <- mapply(
      get_color,
      hist_data$Mortality,
      hist_data$Fuel_Poverty
    )
    
    # Add debugging output
    print("Temporal Data:")
    print(hist_data %>% 
            mutate(
              Mort_Status = ifelse(Mortality > global_medians$mortality_median, "HIGH", "LOW"),
              FP_Status = ifelse(Fuel_Poverty > global_medians$fuel_poverty_median, "HIGH", "LOW")
            ))
    
    # Create plot using directly assigned colors
    ggplot(hist_data, aes(x = 1, y = 1)) +
      facet_wrap(~Year, nrow = 1) +
      geom_tile(aes(fill = fill_color)) +
      scale_fill_identity() +
      theme_void() +
      theme(
        strip.text = element_text(size = 8),
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(2, "points")
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-2, 54, 6)
  })
  
  
  
  ### OBSERVE BLOCK WORKING
  
  
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
      
      pal <- colorBin(
        "YlGn",
        domain = mapped_data[[value_col]],
        bins = 7,
        na.color = "#808080"
      )
      
      map_proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addPolygons(
          data = mapped_data,
          fillColor = ~pal(get(value_col)),
          fillOpacity = 0.7,
          weight = 1,
          color = "#444444",
          label = ~sprintf(
            "<div style='font-family: sans-serif; padding: 8px;'><strong>%s</strong><br/>%s: %s</div>",
            LAD22NM,
            if(input$fuel_metric == "total") "Fuel Poor Households" else "Fuel Poverty Rate",
            if(input$fuel_metric == "total") format(get(value_col), big.mark=",") else paste0(round(get(value_col), 1), "%")
          ) %>% lapply(HTML),
          layerId = ~LAD22CD,
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
          title = if(input$fuel_metric == "total") "Fuel Poor Households" else "Fuel Poverty Rate (%)",
          opacity = 0.7
        )
      
      if(input$show_changes) {
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
              map_proxy %>%
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
      
    } else if(input$view_mode == "mortality") {
      data <- mortality_data()
      if(is.null(data)) return()
      
      mapped_data <- shapes %>%
        left_join(data, by = c("LAD22CD" = "Area_Codes"))
      
      pal <- colorBin(
        "PuBu",
        domain = mapped_data$mortality,
        bins = 7,
        na.color = "#808080"
      )
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data = mapped_data,
          fillColor = ~pal(mortality),
          fillOpacity = 0.7,
          weight = 1,
          color = "#444444",
          label = ~sprintf(
            "<div style='font-family: sans-serif; padding: 8px;'><strong>%s</strong><br/>Mortality Index: %.1f</div>",
            LAD22NM, mortality
          ) %>% lapply(HTML),
          layerId = ~LAD22CD,
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
          values = mapped_data$mortality,
          title = "Winter Mortality Index",
          opacity = 0.7
        )
      
    } else if(input$view_mode == "bivariate") {
    bivariate_data <- get_bivariate_data()
    if(is.null(bivariate_data)) return()
    
    # Join with shape data using explicit columns
    mapped_data <- shapes %>%
        left_join(bivariate_data, by = c("LAD22CD" = "Area_Code"))
    
    # Print debug information for specific areas
    print("East Lindsey Check:")
    print(mapped_data %>% 
        filter(LAD22NM == "East Lindsey") %>% 
        select(LAD22NM, Mortality, Fuel_Poverty, Mortality_Class, Fuel_Poverty_Class))
    
    mapped_data$fill_color <- NA_character_
    
    for(i in 1:nrow(mapped_data)) {
      if(!is.na(mapped_data$Mortality[i]) && !is.na(mapped_data$Fuel_Poverty[i])) {
        mort_class <- mapped_data$Mortality_Class[i]
        fp_class <- mapped_data$Fuel_Poverty_Class[i]
        
        mapped_data$fill_color[i] <- case_when(
          mort_class == "HIGH" && fp_class == "HIGH" ~ "#30585C",
          mort_class == "HIGH" && fp_class == "LOW" ~ "#689E73",
          mort_class == "LOW" && fp_class == "HIGH" ~ "#6277A5",
          TRUE ~ "#D3D3D3"
        )
      }
    }
    
    labels <- lapply(seq_len(nrow(mapped_data)), function(i) {
      if(is.na(mapped_data$Mortality[i]) || is.na(mapped_data$Fuel_Poverty[i])) {
        HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "No data available"
        ))
      } else {
        HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "Winter Mortality Index: ", round(mapped_data$Mortality[i], 1), 
          " (", mapped_data$Mortality_Class[i], ")<br/>",
          "Fuel Poverty: ", round(mapped_data$Fuel_Poverty[i], 1), "%",
          " (", mapped_data$Fuel_Poverty_Class[i], ")"
        ))
      }
    })
    
    # Update map with verified data
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = mapped_data,
        fillColor = ~fill_color,
        fillOpacity = 0.7,
        weight = 1,
        color = "#444444",
        label = labels,
        layerId = ~LAD22CD,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      )
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$view_mode == "mortality") {
      selected_region(input$map_shape_click$id)
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$view_mode == "bivariate") {
      selected_bivariate_region(input$map_shape_click$id)
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