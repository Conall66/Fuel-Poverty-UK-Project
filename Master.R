# Master document


# Packages ----------------------------------------------------------------

# Read data
library(rio)
library(readxl)
library(stringr)

# To clean data
library(tidyverse)
library(lubridate)
library(janitor) # cleans data sets (converts from single object to flattened table)

# Plotting
library(treemap)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(vroom)
library(sf)
library(highcharter)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)

# Collecting Data ---------------------------------------------------------

# Sub regional data on fuel poverty is generated in Sub_Reg_Data_Clean.R and can
# be found in 'Final Data Cleaned' folder

Sub_Reg_Data_12 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2012_LILEE.csv')
Sub_Reg_Data_13 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2013_LILEE.csv')
Sub_Reg_Data_14 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2014_LILEE.csv')
Sub_Reg_Data_15 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2015_LILEE.csv')
Sub_Reg_Data_16 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2016_LILEE.csv')
Sub_Reg_Data_17 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2017_LILEE.csv')
Sub_Reg_Data_18 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2018_LILEE.csv')
Sub_Reg_Data_19 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2019_LILEE.csv')
Sub_Reg_Data_20 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2020_LILEE.csv')
Sub_Reg_Data_21 <- read.csv('Final Data Cleaned/Sub_Reg_Data_2021_LILEE.csv')

# Winter mortality index dataset is generated in
# Winter_Mortality_Data_Clean_Upd.R and can be found in mortality_data.csv

Winter_Mortality_Data <- read.csv('mortality_data.csv')

# UK Shape file read
la_boundaries <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp")


# Plotting ----------------------------------------------------------------

# In this function we iterate through the folder to output the desired file for
# a given year

find_sub_reg_file <- function(folder ,year){
  target_file <- paste0("Sub_Reg_Data_", year, "_LILEE.csv")
  all_files <- list.files(folder, full.names = TRUE)
  matching_file <- all_files[grep(target_file, all_files)] # 
  return(matching_file)
}

# In this function, we will take a year, extract the Winter mortality index with
# area code for that year, extract the proportion of fuel poor homes and area
# codes for that year, and plot the two onto a UK map with 0.5 opacity

map_plt <- function(year){ 
  
  # Extract winter mortality info... column headings contain multiple years -
  # extract using indexing
  
  WMI <- read.csv("mortality_data.csv")
  La_bounds <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp")
  
  start_idx <- 66 # Start index is 66 since WMI data for 2012 starts here
  start_yr <- 2012
  yr_dif <- year - start_yr
  col_idx <- start_idx + (3*yr_dif)
  WMI_val <- colnames(WMI[col_idx])
  WMI_data <- WMI %>%
    select(Area.code, all_of(WMI_val))
  
  # Extract sub regional data based on year by searching through final data
  # cleaned to find the correct file, searching within that file for area code
  # and proportion of fuel poor houses
  
  target_file <- find_sub_reg_file('Final Data Cleaned', year)
  Sub_reg_data <- read.csv(target_file) %>%
    select(Area.Codes, proportion)
  
  # UK map plot info
  uk_boundaries_with_data <- La_bounds %>%
    left_join(WMI_data, by = c("LAD22CD" = "Area.code")) %>%
    left_join(Sub_reg_data, by = c("LAD22CD" = "Area.Codes"))
  
  # Create the dynamic column name
  col_name_WMI <- paste0("Winter.mortality.index..", year, ".", (year + 1))
  
  # Replace column name for WMI
  colnames(uk_boundaries_with_data)[colnames(uk_boundaries_with_data) == col_name_WMI] <- "WMI"
  
  # Replace column name for proportion
  colnames(uk_boundaries_with_data)[colnames(uk_boundaries_with_data) == "proportion"] <- "Sub Regional Data"
  
  
  return(uk_boundaries_with_data)
  
}

# Test input

print(map_plt(2015))

# User Interface ----------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader("Susceptibility to Winter Health Conditions"),
  dashboardSidebar(
    sliderInput("year", "Year:", 2012, 2030, 1),
    width = "10%",
    div(
      style = "font-family: 'Open Sans', sans-serif; font-style: italic; font-weight: bold; display: flex; justify-content: center; align-items: center; height: 100%; padding-top: 50px; gap: 10px;",
      
      noUiSliderInput(
        inputId = "Year_slider",
        min = 50, max = 100,
        value = c(50, 60),
        orientation = "vertical",
        width = "100%",  # Adjust width as needed
        height = "80vh",  # Adjust height as needed
        step = 1,  # Set step to 1 for integer increments
        margin = 10,  # Set margin to 10 for a gap of 10%
        tooltips = TRUE
      )
    )
  ),
  dashboardBody(
    
  )
)

server <- function(input, output, session){
  
  map_data <- reactive({
    map_plt(input$year)}) # Reacts to slider input
  
  fuel_poverty_palette <- colorNumeric(palette = c("white", "blue"), 
                                       domain = map_data$`Sub Regional Data`)
  WMI_palette <- colorNumeric(palette = c("white", "red"), 
                              domain = map_data$`WMI`)
  
  output$map <- renderPlotly({
    
    # Base plot
    p <- ggplot() +
      # Non-England regions in light grey
      geom_sf(data = filter(data, country != "E"),
              fill = "grey90",
              color = "white",
              size = 0.25) +
      
      # England with fuel poverty data
      geom_sf(data = filter(data, country == "E"),
              aes(fill = `Sub Regional Data`,
                  text = paste(LAD22NM, "\nFuel Poverty:", 
                               round(WMI, 1), "%")),
              color = "white",
              size = 0.25,
              alpha = 0.5) + # 0.5 gives 50% opacity to allow winter fuel deaths to be overlaid
      scale_fill_gradientn(
        colors = fuel_poverty_palette(seq(0, 1, length.out = 100)),  # Apply fuel poverty color palette
        name = "% in Fuel Poverty"
      ) +
      
      # England with fuel poverty data
      geom_sf(data = filter(data, country == "E"),
              aes(fill = `WMI`,
                  text = paste(LAD22NM, "\nWinter Mortality Index:", 
                               round(WMI, 1), "%")),
              color = "white",
              size = 0.25,
              alpha = 0.5) + # 0.5 gives 50% opacity to allow winter fuel deaths to be overlaid
      scale_fill_gradientn(
        colors = WMI_palette(seq(0, 1, length.out = 100)),  # Apply WMI poverty color palette
        name = "% WMI"
      ) +
      
      theme_void() +
      labs(title = paste("Fuel Poverty and Winter Mortality in", input$year))
    
    # Convert to plotly for zoom
    ggplotly(p, tooltip = "text") %>%
      layout(dragmode = "zoom")
  })
  
}

# Toggle on and off to test script without running app
shinyApp(ui, server)



