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
library(shiny)


# Program File ------------------------------------------------------------

# Extracting winter mortality index by region and year
# Winter mortality index 

mortality_data <- read_xlsx("wintermortalityreferencetable.xlsx", 
                            sheet = "Table_10",
                            skip = 5) %>%
  # Filter for area codes E06-E09
  filter(str_detect(`Area code`, "^E0[6-9]")) %>%
  # Select relevant columns - area identifiers and just 2011-2022 WMI columns
  select(`Area code`, `Area name`, 
         contains("2011/2012"),
         contains("2012/2013"),
         contains("2013/2014"),
         contains("2014/2015"),
         contains("2015/2016"),
         contains("2016/2017"),
         contains("2017/2018"),
         contains("2018/2019"),
         contains("2019/2020"),
         contains("2020/2021"),
         contains("2021/2022")) %>%
  # Filter for just Winter mortality index columns (not confidence intervals etc)
  select(`Area code`, `Area name`, 
         contains("Winter mortality index")) %>%
  # Replace [z] with 0 in all columns EXCEPT "2020/2021 Excluding COVID-19"
  mutate(across(contains("Winter mortality index") & 
                  !contains("2020/2021 Excluding COVID-19"),
                ~if_else(. == "[z]", "0", as.character(.)))) %>%
  # Convert to numeric
  mutate(across(contains("Winter mortality index"), ~as.numeric(.)))

# Check the column names to verify we have the right years
names(mortality_data)

write.csv(mortality_data, "mortality_data.csv", row.names = TRUE)

