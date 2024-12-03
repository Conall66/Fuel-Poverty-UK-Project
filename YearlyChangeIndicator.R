# Define the function
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
      Absolute_Change = current_year$proportion - previous_year$proportion,
      Percent_Change = ((current_year$proportion - previous_year$proportion) / 
                          previous_year$proportion) * 100
    )
    
    # Add significance indicator
    changes$Significant <- abs(changes$Percent_Change) > 10
    changes$Direction <- ifelse(changes$Absolute_Change > 0, "increase", "decrease")
    
    changes_list[[length(changes_list) + 1]] <- changes
  }
  
  # Combine all changes into one dataframe
  all_changes <- do.call(rbind, changes_list)
  
  return(all_changes)
}



# Run the calculation
changes_data <- calculate_annual_changes()

# View the first few rows
print("First few rows of changes:")
head(changes_data)

# Create histogram
hist(changes_data$Percent_Change, 
     main="Distribution of Year-on-Year Changes",
     xlab="Percent Change")

# Print summary statistics
print("Summary of percent changes:")
summary(changes_data$Percent_Change)
print("\nNumber of significant changes:")
table(changes_data$Significant)
  
  # Look at the most extreme decreases
  extreme_decreases <- changes_data[changes_data$Percent_Change < -40, ]
  print("Regions with largest decreases:")
  print(extreme_decreases[, c("Area_Codes", "Year", "Previous_Year", "Previous_Proportion", "Current_Proportion", "Percent_Change")])

  
  # Look at changes excluding 2019-2020 transition
  non_2020_changes <- changes_data[changes_data$Year != 2020, ]
  summary(non_2020_changes$Percent_Change)
  
  
  # Create periods for analysis
  changes_data$Period <- case_when(
    changes_data$Year < 2019 ~ "Pre-2019",
    changes_data$Year == 2020 ~ "2019-2020",
    TRUE ~ "Post-2020"
  )
  
  # Summary statistics by period
  print("Summary statistics by period:")
  by(changes_data$Percent_Change, changes_data$Period, summary)
  
  # Create boxplots to visualize distribution of changes by period
  boxplot(Percent_Change ~ Period, data = changes_data,
          main = "Distribution of Changes by Time Period",
          ylab = "Percent Change",
          outline = TRUE)  # Show outliers
  
  # Calculate mean changes by period
  aggregate(Percent_Change ~ Period, data = changes_data, mean)
  
  # Count extreme changes (>40% decrease) by period
  extreme_changes <- table(changes_data$Period[changes_data$Percent_Change < -40])
  print("\nNumber of extreme decreases (>40%) by period:")
  print(extreme_changes)