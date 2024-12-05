# Descriptive analytics

# In this file, we look at key information about fuel poverty and winter
# mortality


# Packages ----------------------------------------------------------------

library(summarytools) # Extracting descriptive stats
library(dplyr)
library(tidyr)
library(ggplot2)

# Input Variables ---------------------------------------------------------

start_yr <- 2012
final_yr <- 2021


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
means <- numeric(length(Fuel_Poverty_files))
stds <- numeric(length(Fuel_Poverty_files))
meds <- numeric(length(Fuel_Poverty_files))
iqrs <- numeric(length(Fuel_Poverty_files))

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
  
  means[i] <- meand
  stds[i] <- stdd
  meds[i] <- medd
  iqrs[i] <- iqrd
  
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

dev.off()


# Winter Mortality Data ---------------------------------------------------


