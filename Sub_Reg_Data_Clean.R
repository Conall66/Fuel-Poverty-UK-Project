
# Data Collection and Cleaning


# Packages ----------------------------------------------------------------

# Read data
library(rio)

# To clean data
library(tidyverse)
library(lubridate)
library(janitor) # cleans data sets (converts from single object to flattened table)

# Plotting
library(treemap)

# Key Data sets Import -----------------------------------------------------

LIPEE_conv <- read.csv("Conversion Pattern LIHC to LILEE.csv", header = TRUE)

# Sub Regional Data
# Step 1: Extract sub-regional data-sets into a matrix
# Step 2: Convert files to csv for easier interpretation?
# Step 3: Extract list of area codes
# Step 4: Create csv file with area code, region name, fuel poverty data for 
# each year

Sub_Reg_Files <- list.files("Sub-Regional Data", full.names = TRUE)

# Identify area codes
Area_codes = read_xlsx(Sub_Reg_Files[length(Sub_Reg_Files)], sheet = "Table 2")




