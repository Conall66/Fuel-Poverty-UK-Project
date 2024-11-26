library(dplyr)
library(readr)
library(stringr)

# Read and clean 2020 data
data_2020 <- read_csv("Sub_Reg_extrcsv/Sub_Reg_Data_2020_.csv", 
                      skip = 2,  
                      col_names = c("row", "Area Codes", "Area names", "blank1", "blank2",
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor")) %>%
  # Filter for E codes and remove footnotes
  filter(str_detect(`Area Codes`, "^E")) %>%
  mutate(
    `Number of households` = as.numeric(`Number of households`),
    `Number of households in fuel poverty` = as.numeric(`Number of households in fuel poverty`),
    `Proportion of households fuel poor` = as.numeric(`Proportion of households fuel poor`)
  )

# Clean 2020 data - note this year includes Buckinghamshire changes
standardize_2020 <- function(data) {
  cleaned_data <- data %>%
    select(`Area Codes`, `Number of households`, 
           `Number of households in fuel poverty`, 
           `Proportion of households fuel poor`) %>%
    # Apply only 2020 changes
    mutate(`Area Codes` = case_when(
      `Area Codes` %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",  # Buckinghamshire
      TRUE ~ `Area Codes`
    )) %>%
    group_by(`Area Codes`) %>%
    summarise(
      households = sum(`Number of households`),
      fuel_poor = sum(`Number of households in fuel poverty`),
      proportion = (fuel_poor / households) * 100
    )
  
  return(cleaned_data)
}

# Process 2020 data
standardized_2020 <- standardize_2020(data_2020)
head(standardized_2020)

# Can save if needed:
write_csv(standardized_2020, "Sub_Reg_extrcsv/Sub_Reg_Data_2020_Transitions.csv")