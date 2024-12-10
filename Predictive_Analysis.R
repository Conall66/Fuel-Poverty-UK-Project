
# Predictive Analytics
# Take sub regional data, build linear and non-linear regression models
# Divide existing data into training, test and validation sets
# Determine bias, mean absolute error and RMSE


# Extract Datasets --------------------------------------------------------

Pred_sub_reg <- function(year){
  
  file_name <- paste0("Sub_Reg_Data_", year, "_LILEE.csv")
  file_path <- paste0("Final Data Cleaned/", file_name)
  file <- read.csv(file_path)
  
}

