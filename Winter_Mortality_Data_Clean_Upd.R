
# winter mortality data clean and present


# Packages ----------------------------------------------------------------

# Read data
library(rio)
library(readxl)
library(dplyr)
library(stringr)

# To clean data
library(tidyverse)
library(lubridate)
library(janitor) # cleans data sets (converts from single object to flattened table)

# Plotting
library(treemap)
library(shiny)
library(vroom)
library(sf)
library(ggplot2)
library(dplyr)

# Import Winter Mortality Data --------------------------------------------
mortality_data <- read_xlsx("wintermortalityreferencetable.xlsx", 
                            sheet = "Table_10",
                            skip = 5) %>% # skip first 5 rows
  
  filter(str_detect(`Area code`, "^E0[6-9]")) %>%
  select(
    -starts_with("Winter deaths compared to non-winter deaths"),
    -contains("Including COVID-19")) %>%
  mutate(across(3:ncol(.),
                ~if_else(. == "[z]", "0", as.character(.)))) %>%
  # Convert to numeric
  mutate(across(3:ncol(.), ~as.numeric(.)))
  
zs_detect <- any(str_detect(as.character(mortality_data), "\\[z\\]"))
write.csv(mortality_data, "mortality_data.csv", row.names = FALSE)

# Map ---------------------------------------------------------------------

UK_shp <- "Local_Authority_Districts_December_2022_UK_BGC_V2_8602835163392188905/LAD_DEC_2022_UK_BGC_V2.shp"
la_boundaries <- st_read("Local_Authority_Districts_December_2022/LAD_DEC_2022_UK_BGC_V2.shp")

# Iterate through years of winter deaths

mortality_data_years <- c(seq(1991, 2020, 1))
Area_codes <- mortality_data[1]
Area_names <- mortality_data[2]

# For each year in mortality_data, extract WMI and area code
# Add shapefile plot to folder

year_step <- 1
data_step <- 3

# Create new folder to assign shape plots to, if one does not exist already
shp_plot_WMI <- "shp_plot_WMI"
if (!dir.exists(shp_plot_WMI)){
  dir.create(shp_plot_WMI)
}

# test for loop
# for(year in 1:3){

# real for loop
for(year in mortality_data_years){
  
  year_data <- colnames(mortality_data[data_step])
  
  mortality_data_yr <- mortality_data %>%
    select(`Area code`, `Area name`, all_of(year_data))
  
  uk_boundaries_with_data <- la_boundaries %>%
    left_join(mortality_data_yr, by = c("LAD22CD" = "Area code")) #like VLOOKUP
  
  # Plot with standardized values for better color spread
  gg <- ggplot() + 
    geom_sf(data = uk_boundaries_with_data, 
            aes(fill = uk_boundaries_with_data[[year_data]], 
            color = "white", 
            size = 0.25)) +
    scale_fill_gradient2(low = "blue", mid = "red", high = "yellow", 
                         na.value = "white") +
    theme_void()
  
  # To see the data structure:
  year_label <- gsub("\\D", "", mortality_data_years[year_step])
  plot_name <- paste0("map_", year_label, ".png")
  
  # Save plot to folder
  file_path <- file.path(shp_plot_WMI, plot_name)
  ggsave(file_path, gg)
  
  data_step <- data_step + 3 # Each WMI is 3 rows apart
  year_step <- year_step + 1 # Iterate one further in array of years
}
