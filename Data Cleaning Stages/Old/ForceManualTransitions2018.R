library(dplyr)
library(readr)
library(stringr)  # Add this!

# Read and clean data
data_2018 <- read_csv("Sub_Reg_extrcsv/Sub_Reg_Data_2018_.csv", 
                      skip = 2,  # Skip to row with actual headers
                      col_names = c("row", "LA Code", "LA Name", "Region", 
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor")) %>%
  # Remove rows after 328 and filter for valid LA Codes
  filter(row <= 328, str_detect(`LA Code`, "^E")) %>%
  # Convert numeric columns to numeric type
  mutate(
    `Number of households` = as.numeric(`Number of households`),
    `Number of households in fuel poverty` = as.numeric(`Number of households in fuel poverty`),
    `Proportion of households fuel poor` = as.numeric(`Proportion of households fuel poor`)
  )

standardize_2011_2018 <- function(data) {
  cleaned_data <- data %>%
    select(`LA Code`, `Number of households`, 
           `Number of households in fuel poverty`, 
           `Proportion of households fuel poor`) %>%
    # Apply area code changes
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
  
  return(cleaned_data)
}

# Process the data
standardized_2018 <- standardize_2011_2018(data_2018)
print(head(standardized_2018))

write_csv(standardized_2018, "Sub_Reg_extrcsv/Sub_Reg_Data_2018_Transitions.csv")