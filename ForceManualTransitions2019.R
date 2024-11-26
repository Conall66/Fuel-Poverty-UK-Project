library(dplyr)
library(readr)
library(stringr)

# Read and clean 2019 data
data_2019 <- read_csv("Sub_Reg_extrcsv/Sub_Reg_Data_2019_.csv", 
                      skip = 2,  
                      col_names = c("row", "Area Codes", "Area name", "blank1", "blank2", 
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor")) %>%
  # Keep only rows with E codes and remove footnotes
  filter(str_detect(`Area Codes`, "^E")) %>%
  mutate(
    `Number of households` = as.numeric(`Number of households`),
    `Number of households in fuel poverty` = as.numeric(`Number of households in fuel poverty`),
    `Proportion of households fuel poor` = as.numeric(`Proportion of households fuel poor`)
  )

standardize_2019 <- function(data) {
  cleaned_data <- data %>%
    select(`Area Codes`, `Number of households`, 
           `Number of households in fuel poverty`, 
           `Proportion of households fuel poor`) %>%
    # Apply 2019 changes
    mutate(`Area Codes` = case_when(
      `Area Codes` %in% c("E06000028", "E06000029", "E07000048") ~ "E06000058",  # Bournemouth, Christchurch and Poole
      `Area Codes` %in% c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053") ~ "E06000059",  # Dorset
      `Area Codes` %in% c("E07000205", "E07000206") ~ "E07000244",  # East Suffolk
      `Area Codes` %in% c("E07000201", "E07000204") ~ "E07000245",  # West Suffolk
      `Area Codes` %in% c("E07000190", "E07000191") ~ "E07000246",  # Somerset West and Taunton
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

# Process 2019 data
standardized_2019 <- standardize_2019(data_2019)
print(head(standardized_2019))

write_csv(standardized_2019, "Sub_Reg_extrcsv/Sub_Reg_Data_2019_Transitions.csv")