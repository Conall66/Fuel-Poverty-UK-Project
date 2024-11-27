library(dplyr)
library(readr)

# Read 2020 data
data_2020 <- read_csv("Consistent Area Codes/Sub_Reg_Data_2020_Updated.csv")

# Define all the splits needed
# Cumberland constituents
cumberland_codes <- c("E07000026", "E07000028", "E07000029")
# Westmorland constituents
westmorland_codes <- c("E07000027", "E07000030", "E07000031")
# North Yorkshire constituents
north_yorks_codes <- c("E07000163", "E07000164", "E07000165", "E07000166", 
                       "E07000167", "E07000168", "E07000169")
# Somerset constituents
somerset_codes <- c("E07000187", "E07000188", "E07000189", "E07000246")

# Function to calculate proportions from 2020 data
calculate_proportions <- function(data, codes) {
  data %>%
    filter(`Area Codes` %in% codes) %>%
    mutate(prop_households = households / sum(households),
           prop_fuel_poor = fuel_poor / sum(fuel_poor))
}

# Function to split new authority
split_authority <- function(total_data, props_data, auth_code) {
  total_values <- total_data %>%
    filter(`Area Codes` == auth_code) %>%
    select(`Number of households`, `Number of households in fuel poverty`) %>%
    as.data.frame() %>%
    mutate(across(everything(), as.numeric))
  
  props_data %>%
    mutate(
      households = round(prop_households * total_values$`Number of households`),
      fuel_poor = round(prop_fuel_poor * total_values$`Number of households in fuel poverty`),
      proportion = (fuel_poor / households) * 100
    ) %>%
    select(`Area Codes`, households, fuel_poor, proportion)
}

# Calculate all proportions
cumberland_2020 <- calculate_proportions(data_2020, cumberland_codes)
westmorland_2020 <- calculate_proportions(data_2020, westmorland_codes)
north_yorks_2020 <- calculate_proportions(data_2020, north_yorks_codes)
somerset_2020 <- calculate_proportions(data_2020, somerset_codes)

# Read 2021 data
data_2021 <- read_csv("Data Cleaning Stages/Sub_Reg_First_Pass/Sub_Reg_Data_2021_.csv", 
                      skip = 2,
                      col_names = c("row", "Area Codes", "Area names", "extra1", "extra2",
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor"))

# Split all new authorities
cumberland_split <- split_authority(data_2021, cumberland_2020, "E06000063")
westmorland_split <- split_authority(data_2021, westmorland_2020, "E06000064")
north_yorks_split <- split_authority(data_2021, north_yorks_2020, "E06000065")
somerset_split <- split_authority(data_2021, somerset_2020, "E06000066")

# Process main dataset excluding all new authorities
data_2021_processed <- data_2021 %>%
  filter(!is.na(`Area Codes`), 
         grepl("^E0[6-9]", `Area Codes`),
         !`Area Codes` %in% c("E06000063", "E06000064", "E06000065", "E06000066")) %>%
  select(`Area Codes`,
         `Number of households`,
         `Number of households in fuel poverty`,
         `Proportion of households fuel poor`) %>%
  rename(
    households = `Number of households`,
    fuel_poor = `Number of households in fuel poverty`,
    proportion = `Proportion of households fuel poor`
  ) %>%
  mutate(across(c(households, fuel_poor, proportion), as.numeric))

# Combine all data
data_2021_final <- bind_rows(
  data_2021_processed,
  cumberland_split,
  westmorland_split,
  north_yorks_split,
  somerset_split
) %>%
  arrange(`Area Codes`)

# Save the processed data
write_csv(data_2021_final, "Consistent Area Codes/Sub_Reg_Data_2021_Updated.csv")

# Verify codes match 2020
codes_2020 <- sort(unique(data_2020$`Area Codes`[grep("^E0[6-9]", data_2020$`Area Codes`)]))
codes_2021 <- sort(unique(data_2021_final$`Area Codes`[grep("^E0[6-9]", data_2021_final$`Area Codes`)]))

# Print verification
cat("\nFinal verification:\n")
cat("Codes match 2020 exactly:", identical(codes_2020, codes_2021), "\n")
cat("Number of area codes in 2020:", length(codes_2020), "\n")
cat("Number of area codes in 2021:", length(codes_2021), "\n")

if(!identical(codes_2020, codes_2021)) {
  cat("\nDifferences found:\n")
  cat("In 2020 but not 2021:", paste(setdiff(codes_2020, codes_2021), collapse=", "), "\n")
  cat("In 2021 but not 2020:", paste(setdiff(codes_2021, codes_2020), collapse=", "), "\n")
}