library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

transition_map <- data.frame(
  target_code = c(
    # Northamptonshire split
    "E06000061",  # North Northamptonshire
    "E06000062",  # West Northamptonshire
    
    # Dorset changes
    "E06000058",  # Bournemouth, Christchurch and Poole
    "E06000059",  # Dorset
    
    # Buckinghamshire
    "E06000060",  # Buckinghamshire
    
    # Suffolk changes
    "E07000244",  # East Suffolk
    "E07000245",  # West Suffolk
    
    # Somerset West and Taunton (intermediate change)
    "E07000246"   # Somerset West and Taunton
  ),
  
  target_name = c(
    "North Northamptonshire",
    "West Northamptonshire",
    "Bournemouth, Christchurch and Poole",
    "Dorset",
    "Buckinghamshire",
    "East Suffolk",
    "West Suffolk",
    "Somerset West and Taunton"
  ),
  
  original_codes = c(
    # North Northamptonshire formed from
    "E07000150,E07000152,E07000153,E07000156",
    
    # West Northamptonshire formed from
    "E07000151,E07000154,E07000155",
    
    # Bournemouth, Christchurch and Poole
    "E06000028,E06000029,E07000048",
    
    # Dorset
    "E07000049,E07000050,E07000051,E07000052,E07000053",
    
    # Buckinghamshire
    "E07000004,E07000005,E07000006,E07000007",
    
    # East Suffolk
    "E07000205,E07000206",
    
    # West Suffolk
    "E07000201,E07000204",
    
    # Somerset West and Taunton
    "E07000190,E07000191"
  ),
  
  change_date = as.Date(c(
    "2021-04-01",  # North Northamptonshire
    "2021-04-01",  # West Northamptonshire
    "2019-04-01",  # Bournemouth, Christchurch and Poole
    "2019-04-01",  # Dorset
    "2020-04-01",  # Buckinghamshire
    "2019-04-01",  # East Suffolk
    "2019-04-01",  # West Suffolk
    "2019-04-01"   # Somerset West and Taunton
  ))
)

# Print the transition map
print(transition_map)

standardize_area_codes <- function(data, year) {
  # Convert year to date for comparison (assuming start of year)
  data_date <- as.Date(paste0(year, "-01-01"))
  
  # Sort transitions chronologically
  sorted_transitions <- transition_map %>%
    arrange(change_date)
  
  # Copy of data to transform
  transformed_data <- data
  
  # Apply each transformation if the data date is after the change date
  for(i in 1:nrow(sorted_transitions)) {
    if(data_date >= sorted_transitions$change_date[i]) {
      # Split the original codes string into vector
      old_codes <- unlist(strsplit(sorted_transitions$original_codes[i], ","))
      new_code <- sorted_transitions$target_code[i]
      
      # If any of these old codes exist in our data
      if(any(transformed_data$`Area code` %in% old_codes)) {
        # Aggregate data for areas that were merged
        transformed_data <- transformed_data %>%
          mutate(`Area code` = ifelse(`Area code` %in% old_codes, 
                                      new_code, 
                                      `Area code`)) %>%
          group_by(`Area code`) %>%
          summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
          ungroup()
      }
    }
  }
  
  return(transformed_data)
}

# Example usage:
# For 2018 data (needs all transformations)
# data_2018_standardized <- standardize_area_codes(subregional_data_2018, 2018)

# For 2019 data (needs only post-2019 transformations)
# data_2019_standardized <- standardize_area_codes(subregional_data_2019, 2019)

# For 2020 data (needs only 2021 transformations)
# data_2020_standardized <- standardize_area_codes(subregional_data_2020, 2020)












# Let's check what columns we actually have in one file
example_file <- "Sub_Reg_extrcsv/Sub_Reg_Data_2021_.csv"
data <- read_csv(example_file, skip = 5)  # Skip header rows
print("Column names in the data:")
print(names(data))

# Modify the standardize_area_codes function to use the correct column name
standardize_area_codes <- function(data, year) {
  # Convert year to date for comparison
  data_date <- as.Date(paste0(year, "-01-01"))
  
  # Sort transitions chronologically
  sorted_transitions <- transition_map %>%
    arrange(change_date)
  
  # Copy of data to transform
  transformed_data <- data
  
  # Print the first few rows of data before transformation
  print("First few rows before transformation:")
  print(head(transformed_data))
  
  # Apply each transformation if the data date is after the change date
  for(i in 1:nrow(sorted_transitions)) {
    if(data_date >= sorted_transitions$change_date[i]) {
      # Split the original codes string into vector
      old_codes <- unlist(strsplit(sorted_transitions$original_codes[i], ","))
      new_code <- sorted_transitions$target_code[i]
      
      # Use the correct column name for area codes
      area_code_col <- names(transformed_data)[1]  # Adjust if needed
      
      # If any of these old codes exist in our data
      if(any(transformed_data[[area_code_col]] %in% old_codes)) {
        transformed_data <- transformed_data %>%
          mutate(!!area_code_col := ifelse(!!sym(area_code_col) %in% old_codes, 
                                           new_code, 
                                           !!sym(area_code_col))) %>%
          group_by(!!sym(area_code_col)) %>%
          summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
          ungroup()
      }
    }
  }
  
  return(transformed_data)
}

# Process files
csv_files <- list.files("Sub_Reg_extrcsv", pattern = "*.csv", full.names = TRUE)
standardized_data_list <- list()

for(file in csv_files) {
  year <- get_year(file)
  if(!is.na(year) && year >= 2011 && year <= 2021) {
    cat(sprintf("\nProcessing year %d...\n", year))
    
    # Read the CSV
    yearly_data <- read_csv(file, skip = 5)
    
    # Print structure of data
    cat("Data structure:\n")
    print(str(yearly_data))
    
    # Standardize it
    standardized_data <- standardize_area_codes(yearly_data, year)
    
    # Store in list
    standardized_data_list[[as.character(year)]] <- standardized_data
  }
}