# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

mortality_data <- read_xlsx("wintermortalityreferencetable.xlsx", 
                            sheet = "Table_10",
                            skip = 5) %>%
  # Filter for area codes E06-E09
  filter(str_detect(`Area code`, "^E0[6-9]")) %>%
  # Select relevant columns
  select(`Area code`, `Area name`, 
         contains("Winter mortality index")) %>%
  # Replace [z] with 0 in all columns EXCEPT "2020/2021 Excluding COVID-19"
  mutate(across(contains("Winter mortality index") & 
                  !contains("2020/2021 Excluding COVID-19"),
                ~if_else(. == "[z]", "0", as.character(.)))) %>%
  # Convert to numeric
  mutate(across(contains("Winter mortality index"), ~as.numeric(.)))

# Check results
head(mortality_data)