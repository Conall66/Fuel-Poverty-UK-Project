library(dplyr)
library(readr)
library(stringr)

standardize_historical_data <- function(file_path, year) {
  # Read the data with correct structure for 2011-2017 files
  raw_data <- read_csv(file_path, 
                       skip = 2,
                       col_names = c("row", "LA Code", "LA Name", "Region", 
                                     "Number of households", 
                                     "Number of households in fuel poverty",
                                     "Proportion of households fuel poor")) %>%
    filter(row <= 328, str_detect(`LA Code`, "^E")) %>%
    mutate(across(c("Number of households", 
                    "Number of households in fuel poverty",
                    "Proportion of households fuel poor"), as.numeric))
  
  # Step 1: First wave of consolidations (2018 changes)
  initial_consolidation <- raw_data %>%
    select(`LA Code`, `Number of households`, 
           `Number of households in fuel poverty`) %>%
    # First wave of area code changes
    mutate(`LA Code` = case_when(
      `LA Code` %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",  # Bournemouth, Christchurch and Poole
      `LA Code` %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",  # Dorset
      `LA Code` %in% c("E07000205", "E07000206") ~ "E07000244",  # East Suffolk
      `LA Code` %in% c("E07000201", "E07000204") ~ "E07000245",  # West Suffolk
      `LA Code` %in% c("E07000190", "E07000191") ~ "E07000246",  # Somerset West and Taunton
      TRUE ~ `LA Code`
    )) %>%
    group_by(`LA Code`) %>%
    summarise(
      households = sum(`Number of households`),
      fuel_poor = sum(`Number of households in fuel poverty`),
      proportion = (fuel_poor / households) * 100
    )
  
  # Step 2: Second wave of consolidations and code changes
  final_data <- initial_consolidation %>%
    mutate(`LA Code` = case_when(
      # Northamptonshire reorganization
      `LA Code` %in% c("E07000150", "E07000152", "E07000153", "E07000156") ~ "E06000061",  # North Northants
      `LA Code` %in% c("E07000151", "E07000154", "E07000155") ~ "E06000062",  # West Northants
      # Buckinghamshire reorganization
      `LA Code` %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",  # Buckinghamshire
      # Code changes
      `LA Code` == "E06000048" ~ "E06000057",  # Northumberland
      `LA Code` == "E07000100" ~ "E07000240",  # St Albans
      `LA Code` == "E07000097" ~ "E07000242",  # East Hertfordshire
      `LA Code` == "E07000104" ~ "E07000241",  # Welwyn Hatfield
      `LA Code` == "E08000020" ~ "E08000037",  # Gateshead
      `LA Code` == "E07000101" ~ "E07000243",  # Stevenage
      TRUE ~ `LA Code`
    )) %>%
    group_by(`LA Code`) %>%
    summarise(
      households = sum(households),
      fuel_poor = sum(fuel_poor),
      proportion = (fuel_poor / households) * 100
    ) %>%
    arrange(`LA Code`)
  
  # Create output filename
  output_filename <- paste0("Sub_Reg_Data_", year, "_Updated.csv")
  
  # Save the standardized data
  write_csv(final_data, output_filename)
  
  # Return the processed data
  return(final_data)
}

# Example usage for each year:
# for(year in 2011:2017) {
#   input_file <- paste0("Sub_Reg_Data_", year, "_.csv")
#   standardized_data <- standardize_historical_data(input_file, year)
#   cat("Processed", year, "data\n")
# }

# For all years
for(year in 2011:2017) {
  input_file <- paste0("Sub_Reg_extrcsv/Sub_Reg_Data_", year, "_.csv")
  standardized_data <- standardize_historical_data(input_file, year)
  cat("Processed", year, "data\n")
}