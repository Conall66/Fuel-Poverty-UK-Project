# calculate_region_rankings.R
library(dplyr)

# Load global medians
global_medians <- readRDS("global_medians.rds")

# Initialize data frame for all regions
all_regions <- data.frame(
  Area_Code = character(),
  Area_Name = character(),
  High_High_Count = numeric(),
  Avg_Mortality = numeric(),
  Avg_Fuel_Poverty = numeric(),
  stringsAsFactors = FALSE
)

# Debug counter for verification
debug_counts <- list()

# Process each year
for(year in 2012:2020) {
  # Load mortality data
  mortality_df <- read.csv("WMI_RelevantYears.csv", check.names = FALSE)
  mortality_col <- paste0("Winter mortality index ", year)
  
  # Load fuel poverty data
  fp_file <- sprintf("Final Data Cleaned/Sub_Reg_Data_%d_LILEE.csv", year)
  fp_data <- read.csv(fp_file)
  
  # Create combined dataset with proper matching
  combined_data <- data.frame(
    Area_Code = mortality_df$`Area code`,
    Area_Name = mortality_df$`Area name`,
    Mortality = as.numeric(mortality_df[[mortality_col]]),
    Fuel_Poverty = fp_data$proportion[match(mortality_df$`Area code`, fp_data$Area.Codes)]
  )
  
  # Remove any NA values
  combined_data <- combined_data[complete.cases(combined_data), ]
  
  # Classify using global medians with explicit debugging
  combined_data$High_High <- combined_data$Mortality > global_medians$mortality_median & 
    combined_data$Fuel_Poverty > global_medians$fuel_poverty_median
  
  # Debug output for specific regions
  if(!"Guildford" %in% names(debug_counts)) {
    debug_counts[["Guildford"]] <- data.frame(
      Year = integer(),
      Mortality = numeric(),
      Fuel_Poverty = numeric(),
      Is_High_High = logical(),
      stringsAsFactors = FALSE
    )
  }
  
  # Add to debug counts for Guildford
  guildford_data <- combined_data[combined_data$Area_Name == "Guildford", ]
  if(nrow(guildford_data) > 0) {
    debug_counts[["Guildford"]] <- rbind(
      debug_counts[["Guildford"]],
      data.frame(
        Year = year,
        Mortality = guildford_data$Mortality,
        Fuel_Poverty = guildford_data$Fuel_Poverty,
        Is_High_High = guildford_data$High_High
      )
    )
  }
  
  # Update region statistics
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
    # Match by Area_Code to ensure correct updating
    for(i in 1:nrow(combined_data)) {
      idx <- match(combined_data$Area_Code[i], all_regions$Area_Code)
      if(!is.na(idx)) {
        all_regions$High_High_Count[idx] <- all_regions$High_High_Count[idx] + 
          as.numeric(combined_data$High_High[i])
        all_regions$Avg_Mortality[idx] <- all_regions$Avg_Mortality[idx] + 
          combined_data$Mortality[i]
        all_regions$Avg_Fuel_Poverty[idx] <- all_regions$Avg_Fuel_Poverty[idx] + 
          combined_data$Fuel_Poverty[i]
      }
    }
  }
}

# Print debug information
print("Debug data for Guildford:")
print(debug_counts[["Guildford"]])

# Calculate final metrics
all_regions <- all_regions %>%
  mutate(
    Percentage = (High_High_Count / 9) * 100,
    Avg_Mortality = Avg_Mortality / 9,
    Avg_Fuel_Poverty = Avg_Fuel_Poverty / 9,
    # Modified risk score calculation
    Risk_Score = (Avg_Mortality / mean(Avg_Mortality) + 
                    Avg_Fuel_Poverty / mean(Avg_Fuel_Poverty)) / 2
  ) %>%
  arrange(desc(High_High_Count), desc(Risk_Score)) %>%
  mutate_if(is.numeric, round, 2)

# Save debug information
saveRDS(debug_counts, "debug_counts.rds")

# Save top 10 regions
saveRDS(head(all_regions, 10), "top_regions.rds")

# Print verification for top regions
print("Top 10 regions verification:")
print(head(all_regions, 10))

