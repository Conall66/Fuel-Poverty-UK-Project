# Descriptive analytics

# In this file, we look at key information about fuel poverty and winter
# mortality


# Packages ----------------------------------------------------------------

library(summarytools) # Extracting descriptive stats
library(dplyr)
library(tidyr)
library(ggplot2)
library(ellipse)

# Input Variables ---------------------------------------------------------

start_yr <- 2012
final_yr <- 2020


# Data --------------------------------------------------------------------

Fuel_Poverty_folder <- list("Final Data Cleaned")
Fuel_Poverty_files <- list.files("Final Data Cleaned", full.names = TRUE)


# Fuel Poverty Distribution -----------------------------------------------

# Create folder for boxplots
if(!dir.exists("Boxplots Sub-Regional Data")){
  dir.create("Boxplots Sub-Regional Data")
}

# Create folder for distribution plots
if(!dir.exists("Distribution Sub-Regional Data")){
  dir.create("Distribution Sub-Regional Data")
}

# For all fuel poverty years, determine the mean and std, median and IQR for
# fuel poor proportions. Create boxplots, and distribution curves to represent
# info

Years <- c(seq(start_yr, final_yr, 1))
# Assuming files will only be added with new years after 2021

# Create intialised arrays to store values
means_fp <- numeric(length(Fuel_Poverty_files))
std_fp <- numeric(length(Fuel_Poverty_files))
med_fp <- numeric(length(Fuel_Poverty_files))
iqr_fp <- numeric(length(Fuel_Poverty_files))

numfiles <- length(Fuel_Poverty_files)
proportion_data <- data.frame(Year = numeric(0), proportion = numeric(0))

for(i in 1:numfiles){
  
  file <- Fuel_Poverty_files[i]
  csv_file <- read.csv(file)
  
  if (!"proportion" %in% colnames(csv_file)) {
    print(paste("Column 'proportion' doesn't exist in file:", file))
  } else {
    # Convert 'proportion' column to numeric
    fuel_poor_prop <- as.numeric(csv_file$proportion)
    
    # Calculate statistics and store in the pre-allocated vectors
    meand <- mean(fuel_poor_prop, na.rm = TRUE)  # 'mean()' to calculate mean
    stdd <- sd(fuel_poor_prop, na.rm = TRUE)     # 'sd()' to calculate standard deviation
    medd <- median(fuel_poor_prop, na.rm = TRUE)  # 'median()' to calculate median
    iqrd <- IQR(fuel_poor_prop, na.rm = TRUE)     # 'IQR()' to calculate interquartile range
  }
  
  means_fp[i] <- meand
  std_fp[i] <- stdd
  med_fp[i] <- medd
  iqr_fp[i] <- iqrd
  
  # boxplot
  box_name <- paste0("Fuel Poverty in ", Years[i])
  output_file <- paste0("Boxplots Sub-Regional Data/", box_name, ".png")
  png(output_file)
  
  boxplot(fuel_poor_prop,
          main = box_name,
          ylab = "Proportion of Fuel Poor Houses in England",
          col = "lightBlue"
  )
  
  dev.off()
  
  # distribution curve
  dist_name <- paste0("Distribution of Fuel Poor Houses in ", Years[i])
  output_file_2 <- paste0("Distribution Sub-Regional Data/", dist_name, ".png")
  png(output_file_2)
  
  plot(density(fuel_poor_prop),
       main = dist_name,
       col = "lightBlue", lwd = 2)
  abline(v = medd, col = "red", lwd = 2, lty = 2)  # Red dashed line for the median
  abline(v = meand, col = "blue", lwd = 2, lty = 2)
  abline(v = meand - stdd, col = "grey", lwd = 2, lty = 2)
  abline(v = meand + stdd, col = "grey", lwd = 2, lty = 2)
  
  legend("topright",  # Position of the legend
         legend = c("Density", "Median", "Mean", "Mean ± 1 SD"),  # Labels
         col = c("lightBlue", "red", "blue", "grey"),  # Line colors
         lty = c(1, 2, 1, 2),  # Line types (solid, dashed, etc.)
         lwd = c(2, 2, 2, 2))  # Line widths
  
  dev.off()
  
  # add proportions to initialised dataframe
  
  proportion_data <- rbind(proportion_data, 
                           data.frame(Year = Years[i], proportion = fuel_poor_prop))
  
}


# boxplot
box_name <- paste0("Fuel Poverty in ", Years[1], "-", Years[length(Years)])
output_file <- paste0("Boxplots Sub-Regional Data/", box_name, ".png")
png(output_file)

boxplot(proportion_data$proportion,
        main = box_name,
        ylab = "Proportion of Fuel Poor Houses in England",
        col = "lightBlue"
)

dev.off()

# distribution curve
dist_name <- paste0("Distribution of Fuel Poor Houses in ", Years[1], "-", Years[length(Years)])
output_file_2 <- paste0("Distribution Sub-Regional Data/", dist_name, ".png")
png(output_file_2)

plot(density(proportion_data$proportion),
     main = dist_name,
     col = "lightBlue", lwd = 2)
abline(v = median(proportion_data$proportion), col = "red", lwd = 2, lty = 2)
abline(v = mean(proportion_data$proportion), col = "blue", lwd = 2, lty = 2)
abline(v = mean(proportion_data$proportion) - sd(proportion_data$proportion), 
       col = "grey", lwd = 2, lty = 2)
abline(v = mean(proportion_data$proportion) + sd(proportion_data$proportion), 
       col = "grey", lwd = 2, lty = 2)

legend("topright",  # Position of the legend
       legend = c("Density", "Median", "Mean", "Mean ± 1 SD"),  # Labels
       col = c("lightBlue", "red", "blue", "grey"),  # Line colors
       lty = c(1, 2, 1, 2),  # Line types (solid, dashed, etc.)
       lwd = c(2, 2, 2, 2))  # Line widths

dev.off()


# Winter Mortality Data ---------------------------------------------------

WMI_data <- read.csv("mortality_data.csv")

# Calculate mean, sd, median and iqr for each year, boxplots and ditribution
# curves for each, then collate total plots

# Create folder for boxplots
if(!dir.exists("Boxplots WMI Data")){
  dir.create("Boxplots WMI Data")
}

# Create folder for distribution plots
if(!dir.exists("Distribution WMI Data")){
  dir.create("Distribution WMI Data")
}

# Create intialised arrays to store values
means_wmi <- numeric(length(Years))
std_wmi <- numeric(length(Years))
med_wmi <- numeric(length(Years))
iqr_wmi <- numeric(length(Years))

# numfiles <- length(Fuel_Poverty_files)
proportion_data_WMI <- data.frame(Year = numeric(0), WMI = numeric(0))

upd_idx <- 66 # On this column that 2012 data begins

for(i in 1:(length(Years))){
  
    col_select <- as.numeric(unlist(WMI_data[upd_idx]))
    
    # Calculate statistics and store in the pre-allocated vectors
    meand <- mean(col_select, na.rm = TRUE)  # 'mean()' to calculate mean
    stdd <- sd(col_select, na.rm = TRUE)     # 'sd()' to calculate standard deviation
    medd <- median(col_select, na.rm = TRUE)  # 'median()' to calculate median
    iqrd <- IQR(col_select, na.rm = TRUE)     # 'IQR()' to calculate interquartile range
  
    # Update separate arrays
    means_wmi[i] <- meand
    std_wmi[i] <- stdd
    med_wmi[i] <- medd
    iqr_wmi[i] <- iqrd
  
    # boxplot
    box_name <- paste0("Winter Mortality Index in ", Years[i])
    output_file <- paste0("Boxplots WMI Data/", box_name, ".png")
    png(output_file)
    
    boxplot(col_select,
            main = box_name,
            ylab = "Winter Mortality Index",
            col = "lightBlue"
    )
    
    dev.off()
    
    # distribution curve
    dist_name <- paste0("Winter Mortality Index Distribution ", Years[i])
    output_file_2 <- paste0("Distribution WMI Data/", dist_name, ".png")
    png(output_file_2)
    
    plot(density(col_select),
         main = dist_name,
         col = "lightBlue", lwd = 2)
    abline(v = medd, col = "red", lwd = 2, lty = 2)  # Red dashed line for the median
    abline(v = meand, col = "blue", lwd = 2, lty = 2)
    abline(v = meand - stdd, col = "grey", lwd = 2, lty = 2)
    abline(v = meand + stdd, col = "grey", lwd = 2, lty = 2)
    
    legend("topright",  # Position of the legend
           legend = c("Density", "Median", "Mean", "Mean ± 1 SD"),  # Labels
           col = c("lightBlue", "red", "blue", "grey"),  # Line colors
           lty = c(1, 2, 1, 2),  # Line types (solid, dashed, etc.)
           lwd = c(2, 2, 2, 2))  # Line widths
    
    dev.off()
    
    # add proportions to initialised dataframe
    # Ensure `year_values` has the same length as `col_select`
    year_values <- rep(Years[i], length(col_select))
    
    # Add proportions to the data frame
    proportion_data_WMI <- rbind(proportion_data_WMI, 
                                 data.frame(Year = year_values, WMI = col_select))
    
    upd_idx <- upd_idx + 3
    
}


# boxplot
box_name <- paste0("Winter Mortality Index in ", Years[1], "-", Years[length(Years)])
output_file <- paste0("Boxplots WMI Data/", box_name, ".png")
png(output_file)

boxplot(proportion_data_WMI$WMI,
        main = box_name,
        ylab = "Winter Mortality Index",
        col = "lightBlue"
)

dev.off()

# distribution curve
dist_name <- paste0("Winter Mortality Index Values in ", Years[1], "-", Years[length(Years)])
output_file_2 <- paste0("Distribution WMI Data/", dist_name, ".png")
png(output_file_2)

plot(density(proportion_data_WMI$WMI),
     main = dist_name,
     col = "lightBlue", lwd = 2)
abline(v = median(proportion_data_WMI$WMI), col = "red", lwd = 2, lty = 2)
abline(v = mean(proportion_data_WMI$WMI), col = "blue", lwd = 2, lty = 2)
abline(v = mean(proportion_data_WMI$WMI) - sd(proportion_data_WMI$WMI), 
       col = "grey", lwd = 2, lty = 2)
abline(v = mean(proportion_data_WMI$WMI) + sd(proportion_data_WMI$WMI), 
       col = "grey", lwd = 2, lty = 2)

legend("topright",  # Position of the legend
       legend = c("Density", "Median", "Mean", "Mean ± 1 SD"),  # Labels
       col = c("lightBlue", "red", "blue", "grey"),  # Line colors
       lty = c(1, 2, 1, 2),  # Line types (solid, dashed, etc.)
       lwd = c(2, 2, 2, 2))  # Line widths

dev.off()

# WMI and Fuel Poverty Relationship ---------------------------------------

# Plot average winter mortality values and average fuel poverty values by year,
# use Spearman's Rank to evaluate strength of correlation

# Create folder

# Create folder for distribution plots
if(!dir.exists("Relationship WMI Fuel Pov")){
  dir.create("Relationship WMI Fuel Pov")
}

# Initialised dataframe for storing spearman's rank info
SP_vals <- data.frame(Spearmans = numeric(length(Years)-1), Stat_sign = numeric(length(Years)-1))

upd_idx <- 66

for (item in (1:(length(Years)-1))){
  year <- Years[item]
  # Extract fuel poverty data
  fuel_pov_file <- paste0("Final Data Cleaned/Sub_Reg_Data_", year, "_LILEE.csv")
  fuel_pov <- read.csv(fuel_pov_file)
  fuel_pov_prop_2 <- as.numeric(fuel_pov$proportion)
  
  # Extract WMI value
  col_select_2 <- as.numeric(unlist(WMI_data[upd_idx]))
  
  combined_data <- data.frame(fuel_pov_prop_2, col_select_2)
  
  # Spearmans Rank
  spearman_corr <- cor(combined_data$fuel_pov_prop_2, combined_data$col_select_2, method = "spearman")
  # Spearmans Rank
  spearman_cor_test <- cor.test(combined_data$fuel_pov_prop_2, combined_data$col_select_2, 
                                method = "spearman",
                                exact = FALSE,
                                alternative = "two.sided")
  # Upd dataframe
  SP_vals$Spearmans[item] <- spearman_corr
  SP_vals$Stat_sign[item] <- spearman_cor_test$p.value
  
  # Plot against one another
  ggplot(combined_data, aes(x = fuel_pov_prop_2, y = col_select_2)) +
    geom_point(color = "blue", size = 3) +  # Scatter points
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
    labs(
      title = "Scatter Plot of Fuel Poverty vs Winter Mortality",
      x = "Fuel Poverty (%)",
      y = "Winter Mortality Index"
    ) +
    annotate(
      "text", 
      x = max(combined_data$fuel_pov_prop_2) * 0.7, 
      y = max(combined_data$col_select_2) * 0.9,
      label = paste("Spearman's rho =", round(spearman_corr, 2)),
      color = "darkred"
    ) +
    theme_classic()
    
  file_name <- paste0("Scatter Plot ", year)
  file_loc <- paste0("Relationship WMI Fuel Pov/", year, ".png")
  ggsave(file_loc)
  
  upd_idx <- upd_idx + 3
  
}

# UK Temperature and Correlation ------------------------------------------

# Categorised by year
Temp_correlation <- read.csv("UK_Temp.csv")

Temp_FP_WMI <- data.frame(Fuel_Poverty = means_fp[1:(length(means_fp)-1)], 
                          WMI = means_wmi, 
                          Temp = Temp_correlation$Min_Temp[1:(length(Temp_correlation$Min_Temp)-1)])
FP_WMI_cor <- cor(Temp_FP_WMI$Fuel_Poverty, Temp_FP_WMI$WMI, method = "spearman")
FP_Temp_cor <- cor(Temp_FP_WMI$Fuel_Poverty, Temp_FP_WMI$Temp, method = "spearman")
WMI_Temp_cor <- cor(Temp_FP_WMI$WMI, Temp_FP_WMI$Temp, method = "spearman")


