# calculate_changes.R
library(dplyr)

calculate_annual_changes <- function(folder_path = "Final Data Cleaned") {
  # List all fuel poverty data files
  files <- list.files(folder_path, pattern = "Sub_Reg_Data_.*_LILEE\\.csv", full.names = TRUE)
  
  # Sort files by year
  years <- as.numeric(gsub(".*_([0-9]{4})_.*", "\\1", files))
  files <- files[order(years)]
  
  # Create empty list to store changes
  changes_list <- list()
  
  # Calculate changes between consecutive years
  for(i in 2:length(files)) {
    current_year <- read.csv(files[i])
    previous_year <- read.csv(files[i-1])
    
    # Calculate changes
    changes <- data.frame(
      Area_Codes = current_year$Area.Codes,
      Year = years[i],
      Previous_Year = years[i-1],
      Current_Proportion = current_year$proportion,
      Previous_Proportion = previous_year$proportion,
      Percent_Change = ((current_year$proportion - previous_year$proportion) / 
                          previous_year$proportion) * 100,
      Significant = FALSE,  # Will update after
      Direction = "none"    # Will update after
    )
    
    # Add significance indicator (>20% change)
    changes$Significant <- abs(changes$Percent_Change) > 20
    changes$Direction <- ifelse(changes$Percent_Change > 0, "increase", "decrease")
    
    changes_list[[length(changes_list) + 1]] <- changes
  }
  
  # Combine all changes into one dataframe
  all_changes <- do.call(rbind, changes_list)
  
  # Save to CSV
  write.csv(all_changes, "fuel_poverty_changes.csv", row.names = FALSE)
  
  return(all_changes)
}

# Run the calculation
changes <- calculate_annual_changes()