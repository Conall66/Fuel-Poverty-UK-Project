# Data Collection and Cleaning


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

# Key Data sets Import -----------------------------------------------------

# Read the excel file, skipping the header rows
winter_mortality_data = read_xlsx("wintermortalityreferencetable.xlsx", 
                                  sheet = "Table_10",  # Using the exact sheet name I can see
                                  skip = 5)  # Skip the headers to start at the data

# Clean and prepare the data
mortality_clean <- winter_mortality_data %>%
  # Select area code, area name, and all Winter Mortality Index columns
  select(1, 2, 
         contains("Winter mortality index")) %>%  #contains() to catch all index columns%>%
  # Filter for area codes starting with E06, E07, E08, or E09
  filter(str_detect(`Area code`, "^E0[6-9]"))
  # Convert [z] and any other non-numeric values to NA
  mutate(across(-c(1:2), ~ifelse(. %in% c("[z]", "z"), NA, as.numeric(.)))) 

# Now let's do interpolation by group (area code)
library(tidyr)
library(zoo)  # for na.approx function

mortality_interpolated <- winter_mortality_data %>%
  # Convert to long format to make interpolation easier
  pivot_longer(cols = -c(`Area code`, `Area name`),
               names_to = "year",
               values_to = "mortality_index") %>%
  # Group by area code so interpolation happens within each area
  group_by(`Area code`) %>%
  # Apply linear interpolation
  mutate(
    mortality_index_interpolated = na.approx(mortality_index, na.rm = FALSE)
  ) %>%
  # Convert back to wide format
  pivot_wider(
    names_from = "year",
    values_from = "mortality_index_interpolated"
  )

# Look at the results
head(mortality_interpolated)