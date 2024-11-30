# Read conversion pattern
conversion_pattern <- read.csv("Conversion Pattern LIHC to LILEE.csv")

# Function to update a single year's data
update_fuel_poverty <- function(year) {
  # Get multiplier for this year
  multiplier <- conversion_pattern$Multiplier..LILEE.LIHC.[conversion_pattern$Year == year]
  
  if(length(multiplier) == 0) {
    print(paste("No multiplier found for year", year))
    return(NULL)
  }
  
  # Read the data file
  filename <- paste0("Consistent Area Codes/Sub_Reg_Data_", year, "_Updated.csv")
  data <- read.csv(filename)
  
  # Store original values
  original_fuel_poor <- data$fuel_poor
  
  # Update fuel poor numbers and proportion
  data$fuel_poor <- round(data$fuel_poor * multiplier)
  data$proportion <- (data$fuel_poor / data$households) * 100
  
  # Create a summary of changes
  summary_stats <- data.frame(
    Year = year,
    Original_Total = sum(original_fuel_poor),
    New_Total = sum(data$fuel_poor),
    Original_Mean_Proportion = mean(data$proportion / multiplier),
    New_Mean_Proportion = mean(data$proportion)
  )
  
  # Write updated data
  write.csv(data, paste0("Consistent Area Codes/Sub_Reg_Data_", year, "_Updated_LILEE.csv"), 
            row.names = FALSE)
  
  return(summary_stats)
}

# Process years 2012-2019
years <- 2012:2019
summaries <- lapply(years, update_fuel_poverty)

# Combine summaries into one dataframe
all_summaries <- do.call(rbind, summaries)
print(all_summaries)