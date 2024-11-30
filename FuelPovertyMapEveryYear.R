# Load required libraries
library(sf)
library(ggplot2)
library(dplyr)

# Read the LA boundaries shapefile
la_boundaries <- st_read("Local_Authority_Districts_December_2022_UK_BGC_V2_8602835163392188905")

# Function to create map for a specific year
create_fuel_poverty_map <- function(year) {
  # Read the data for the specific year
  filename <- paste0("Final Data Cleaned/Sub_Reg_Data_", year, "_LILEE.csv")
  
  # Read CSV
  fuel_data <- read.csv(filename)
  
  # Join with boundaries
  uk_boundaries_with_data <- la_boundaries %>%
    left_join(fuel_data, by = c("LAD22CD" = "Area.Codes"))
  
  # Create the plot
  gg <- ggplot() + 
    geom_sf(data = uk_boundaries_with_data, 
            aes(fill = proportion), 
            color = "white", 
            size = 0.25) +
    scale_fill_gradient2(
      low = "blue", 
      mid = "green",
      high = "red",
      midpoint = median(fuel_data$proportion, na.rm = TRUE),  # Changed to median for better color distribution
      na.value = "grey80",
      name = "% in Fuel Poverty"
    ) +
    theme_void() +
    labs(title = paste("Fuel Poverty in", year))
  
  # Save the plot
  ggsave(paste0("fuel_poverty_map_", year, ".png"), gg, width = 8, height = 10)
  
  return(gg)
}

# Create maps for each year from 2012 to 2021
years <- 2012:2021
maps <- lapply(years, create_fuel_poverty_map)