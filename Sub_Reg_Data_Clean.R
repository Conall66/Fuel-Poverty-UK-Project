
# Data Collection and Cleaning


# Packages ----------------------------------------------------------------

# Read data
library(rio)
library(readxl)

# To clean data
library(tidyverse)
library(lubridate)
library(janitor) # cleans data sets (converts from single object to flattened table)

# Plotting
library(treemap)
library(shiny)
library(sf)

# Key Data sets Import -----------------------------------------------------

LIPEE_conv <- read.csv("Conversion Pattern LIHC to LILEE.csv", header = TRUE)

# Sub Regional Data
# Step 1: Extract sub-regional data-sets into a matrix
# Step 2: Convert files to csv for easier interpretation?
# Step 3: Extract list of area codes
# Step 4: Create csv file with area code, region name, fuel poverty data for 
# each year

Sub_Reg_Files <- list.files("Sub-Regional Data", full.names = TRUE)

# Identify area codes from latest file
Sub_Reg_Data_22 <- read_xlsx(Sub_Reg_Files[length(Sub_Reg_Files)], sheet = "Table 2")
Area_Codes <- Sub_Reg_Data_22[3:nrow(Sub_Reg_Data_22), 1] #First column is area codes
# Extract first 2 columns as empty text

# Extract useful information on fuel poverty data by area code per dataset
file_yr <- 2011 # Initialise year

# Create the folder if it doesn't already exist
if (!dir.exists("Sub_Reg_extrcsv")) {
  dir.create("Sub_Reg_extrcsv")
}

# DO NOT HAVE EXCEL FILES OPEN WHILST RUNNING
for(file in Sub_Reg_Files){
  
  file_name <- paste("Sub_Reg_Data", file_yr, ".csv", sep = "_")
  file_path <- file.path("Sub_Reg_extrcsv", file_name)
  
  # Navigate to correct sheet
  if(file_yr == 2011){
    file_sht <- read_xlsx(file, "Local Authority") #Called something different for 2011
  } else{
    file_sht <- read_xlsx(file, "Table 2")
  }
  # Extract information by area code for fuel poverty
  # Create new csv file with only useful info
  write.csv(file_sht, file_path)
  file_yr <- file_yr + 1
  
  # num_rows <- nrow(file_sht)
  # # Print the number of rows for the current file
  # print(paste("File:", file, "has", num_rows, "rows"))
  
}

# Comparing Datasets ------------------------------------------------------

# Check the sub-regional datasets have no missing data
# Check area codes and transpose to match


# Area Code Scale Up ------------------------------------------------------

# Concerting area code to district, to county etc. for expanding out


# Mapping -----------------------------------------------------------------

# Create a map with each postcode highlighted in colour communicating proportion
# living in fuel poverty with year slider (moving between datasets)

uk_shape <- 

# Predictive Analytics ----------------------------------------------------

# Determine the most appropriate way to map changes in fuel poverty going forward

# Hello

