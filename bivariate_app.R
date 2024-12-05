# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(classInt)
library(ggplot2)

# UI Definition
ui <- fluidPage(
  titlePanel("Fuel Poverty and Winter Mortality Bivariate Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Year selector
      sliderInput("year", 
                  "Select Year:",
                  min = 2012,
                  max = 2021,
                  value = 2012,
                  sep = ""),
      
      # Add explanation text
      helpText("This map shows the relationship between fuel poverty rates",
               "and winter mortality index across England.",
               "Areas are colored based on both metrics combined."),
      
      # Warning message for missing data
      htmlOutput("warning_message"),
      
      # Summary statistics output
      h4("Summary Statistics"),
      verbatimTextOutput("summary_stats")
    ),
    
    mainPanel(
      # Main map
      leafletOutput("map", height = "500px"),
      
      # Improved bivariate legend using ggplot
      plotOutput("bivariate_legend", height = "200px", width = "200px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Load and process data
  get_combined_data <- reactive({
    year_selected <- input$year
    # Read mortality data
    mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
    
    # Construct fuel poverty file path for selected year
    year_selected <- input$year
    fp_filepath <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year_selected)
    
    # Check if fuel poverty file exists
    if (!file.exists(fp_filepath)) {
      return(NULL)
    }
    
    # Read fuel poverty data
    fuel_poverty_df <- read.csv(fp_filepath)
    
    # Get mortality column for selected year
    mortality_col <- paste0("Winter mortality index ", year_selected)
    
    # Check if mortality data exists for selected year
    if(!mortality_col %in% names(mortality_df)) {
      return(NULL)
    }
    
    if(year_selected == 2021) {
      return(NULL)
    }
    
    # Combine data
    combined_data <- data.frame(
      Area_Code = mortality_df$`Area code`,
      Area_Name = mortality_df$`Area name`,
      Mortality = as.numeric(mortality_df[[mortality_col]]),
      Fuel_Poverty = fuel_poverty_df$proportion
    )
    
    # Remove rows with NA
    combined_data <- combined_data[complete.cases(combined_data), ]
    
    # Add tercile classifications
    combined_data$Mortality_Class <- cut(combined_data$Mortality, 
                                         breaks = quantile(combined_data$Mortality, 
                                                           probs = c(0, 1/3, 2/3, 1), 
                                                           na.rm = TRUE),
                                         labels = 1:3,
                                         include.lowest = TRUE)
    
    combined_data$Fuel_Poverty_Class <- cut(combined_data$Fuel_Poverty, 
                                            breaks = quantile(combined_data$Fuel_Poverty, 
                                                              probs = c(0, 1/3, 2/3, 1), 
                                                              na.rm = TRUE),
                                            labels = 1:3,
                                            include.lowest = TRUE)
    
    return(combined_data)
  })
  
  # Create bivariate color matrix
  bivariate_color_matrix <- reactive({
    # Create a 3x3 matrix of colors
    matrix(
      c("#e8e8e8", "#b4d3e8", "#3c9ac7",   # Low mortality
        "#e4c1a9", "#9fb0d3", "#2c7bb6",    # Medium mortality
        "#df9a87", "#8c8dbe", "#1c4ba0"),   # High mortality
      nrow = 3, 
      byrow = TRUE
    )
  })
  
  # Load shape data
  shape_data <- reactive({
    tryCatch({
      sf_data <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp", quiet = TRUE) %>%
        st_transform(4326)
      return(sf_data)
    }, error = function(e) {
      print(paste("Error reading shapefile:", e$message))
      return(NULL)
    })
  })
  
  # Improved bivariate legend using ggplot2
  output$bivariate_legend <- renderPlot({
    # Create a data frame for the legend
    legend_data <- expand.grid(
      x = 1:3,
      y = 1:3
    )
    
    # Add colors
    legend_data$fill <- as.vector(bivariate_color_matrix())
    
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
  
  # Update map
  output$map <- renderLeaflet({
    combined_data <- get_combined_data()
    shapes <- shape_data()
    
    if(is.null(combined_data) || is.null(shapes)) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = -2, lat = 54, zoom = 6))
    }
    
    # Join data with shapes
    mapped_data <- shapes %>%
      left_join(combined_data, by = c("LAD22CD" = "Area_Code"))
    
    # Create bivariate colors
    mapped_data$fill_color <- NA
    for(i in 1:nrow(mapped_data)) {
      if(!is.na(mapped_data$Mortality_Class[i]) && !is.na(mapped_data$Fuel_Poverty_Class[i])) {
        mapped_data$fill_color[i] <- bivariate_color_matrix()[
          as.numeric(mapped_data$Mortality_Class[i]),
          as.numeric(mapped_data$Fuel_Poverty_Class[i])
        ]
      }
    }
    
    # Create labels
    labels <- lapply(seq_len(nrow(mapped_data)), function(i) {
      if(is.na(mapped_data$Mortality[i]) || is.na(mapped_data$Fuel_Poverty[i])) {
        return(HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "No data available"
        )))
      } else {
        return(HTML(paste0(
          "<b>", mapped_data$LAD22NM[i], "</b><br/>",
          "Winter Mortality Index: ", round(mapped_data$Mortality[i], 1), "<br/>",
          "Fuel Poverty: ", round(mapped_data$Fuel_Poverty[i], 1), "%"
        )))
      }
    })
    
    # Create map
    leaflet(mapped_data) %>%
      addTiles() %>%
      setView(lng = -2, lat = 54, zoom = 6) %>%
      addPolygons(
        fillColor = ~fill_color,
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
      )
  })
  
  # Warning message
  output$warning_message <- renderUI({
    year_selected <- input$year
    
    if(year_selected == 2021) {
      div(
        style = "color: red; background-color: #ffe6e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        "No winter mortality data available for 2021"
      )
    } else if(is.null(get_combined_data())) {
      div(
        style = "color: red; background-color: #ffe6e6; padding: 10px; border-radius: 5px; margin-top: 10px;",
        icon("exclamation-triangle"),
        "No data available for", year_selected
      )
    }
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    combined_data <- get_combined_data()
    if(!is.null(combined_data) && nrow(combined_data) > 0) {
      paste(
        "Average winter mortality index:", round(mean(combined_data$Mortality, na.rm = TRUE), 2), "\n",
        "Average fuel poverty rate:", round(mean(combined_data$Fuel_Poverty, na.rm = TRUE), 2), "%\n",
        "Correlation coefficient:", round(cor(combined_data$Mortality, 
                                              combined_data$Fuel_Poverty, 
                                              use = "complete.obs"), 3)
      )
    } else {
      "No data available for selected year"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)