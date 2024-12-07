# calculate_region_rankings.R

library(dplyr)

# Load global medians
global_medians <- readRDS("global_medians.rds")

# Initialize data frame
all_regions <- data.frame(
  Area_Code = character(),
  Area_Name = character(),
  High_High_Count = numeric(),
  Avg_Mortality = numeric(),
  Avg_Fuel_Poverty = numeric(),
  stringsAsFactors = FALSE
)

# For each year
for(year in 2012:2020) {
  mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
  mortality_col <- paste0("Winter mortality index ", year)
  
  fp_file <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year)
  fp_data <- read.csv(fp_file)
  
  combined_data <- data.frame(
    Area_Code = mortality_df$`Area code`,
    Area_Name = mortality_df$`Area name`,
    Mortality = as.numeric(mortality_df[[mortality_col]]),
    Fuel_Poverty = fp_data$proportion
  )
  
  # Classify using global medians
  combined_data$High_High <- combined_data$Mortality > global_medians$mortality_median & 
    combined_data$Fuel_Poverty > global_medians$fuel_poverty_median
  
  # Update counts and averages
  if(nrow(all_regions) == 0) {
    all_regions <- data.frame(
      Area_Code = combined_data$Area_Code,
      Area_Name = combined_data$Area_Name,
      High_High_Count = as.numeric(combined_data$High_High),
      Avg_Mortality = combined_data$Mortality,
      Avg_Fuel_Poverty = combined_data$Fuel_Poverty,
      stringsAsFactors = FALSE
    )
  } else {
    all_regions$High_High_Count <- all_regions$High_High_Count + as.numeric(combined_data$High_High)
    all_regions$Avg_Mortality <- all_regions$Avg_Mortality + combined_data$Mortality
    all_regions$Avg_Fuel_Poverty <- all_regions$Avg_Fuel_Poverty + combined_data$Fuel_Poverty
  }
}

# Calculate final averages and scores
all_regions <- all_regions %>%
  mutate(
    Percentage = (High_High_Count / 9) * 100,
    Avg_Mortality = Avg_Mortality / 9,
    Avg_Fuel_Poverty = Avg_Fuel_Poverty / 9,
    # Create a composite score combining both metrics
    Risk_Score = (Avg_Mortality / mean(Avg_Mortality) + Avg_Fuel_Poverty / mean(Avg_Fuel_Poverty)) / 2
  ) %>%
  # Sort first by High_High_Count, then by Risk_Score for ties
  arrange(desc(High_High_Count), desc(Risk_Score)) %>%
  mutate_if(is.numeric, round, 2)

# Save top 10 to RDS
saveRDS(head(all_regions, 10), "top_regions.rds")