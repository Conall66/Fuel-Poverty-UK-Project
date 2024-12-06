# calculate_global_medians.R

library(dplyr)

# Initialize vectors to store all values
all_mortality <- numeric()
all_fuel_poverty <- numeric()

# Read mortality data
mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)

# Collect all mortality values from 2012-2020
for(year in 2012:2020) {
  mortality_col <- paste0("Winter mortality index ", year)
  if(mortality_col %in% names(mortality_df)) {
    all_mortality <- c(all_mortality, as.numeric(mortality_df[[mortality_col]]))
  }
}

# Collect all fuel poverty values from 2012-2020
for(year in 2012:2020) {
  fp_file <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year)
  if(file.exists(fp_file)) {
    fp_data <- read.csv(fp_file)
    all_fuel_poverty <- c(all_fuel_poverty, fp_data$proportion)
  }
}

# Calculate medians
global_medians <- list(
  mortality_median = median(all_mortality, na.rm = TRUE),
  fuel_poverty_median = median(all_fuel_poverty, na.rm = TRUE)
)

# Print results
print("Global Medians:")
print(paste("Mortality Median:", round(global_medians$mortality_median, 2)))
print(paste("Fuel Poverty Median:", round(global_medians$fuel_poverty_median, 2), "%"))

# Save to RDS file for easy loading in the app
saveRDS(global_medians, "global_medians.rds")

