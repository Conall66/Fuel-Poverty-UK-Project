# Read the data
data_2018 <- read.csv("Sub_Reg_Data_2018_Updated.csv")
data_2019 <- read.csv("Sub_Reg_Data_2019_Updated.csv")
data_2020 <- read.csv("Sub_Reg_extrcsv/Sub_Reg_Data_2020_Transitions.csv")

# Extract E06-E09 codes from each year
codes_2018 <- sort(unique(data_2018$LA.Code[grep("^E0[6-9]", data_2018$LA.Code)]))
codes_2019 <- sort(unique(data_2019$Area.Codes[grep("^E0[6-9]", data_2019$Area.Codes)]))
codes_2020 <- sort(unique(data_2020$Area.Codes[grep("^E0[6-9]", data_2020$Area.Codes)]))

# Compare lengths
cat("Number of E06-E09 codes in each year:\n")
cat("2018:", length(codes_2018), "\n")
cat("2019:", length(codes_2019), "\n")
cat("2020:", length(codes_2020), "\n")

# Check if all codes are identical across years
all_match <- identical(codes_2018, codes_2019) && identical(codes_2019, codes_2020)
cat("\nDo all codes match exactly across years? ", all_match, "\n")

# If they don't match, find the differences
if(!all_match) {
  cat("\nDifferences between 2018 and 2019:\n")
  cat("In 2018 but not 2019:", paste(setdiff(codes_2018, codes_2019), collapse=", "), "\n")
  cat("In 2019 but not 2018:", paste(setdiff(codes_2019, codes_2018), collapse=", "), "\n")
  
  cat("\nDifferences between 2019 and 2020:\n")
  cat("In 2019 but not 2020:", paste(setdiff(codes_2019, codes_2020), collapse=", "), "\n")
  cat("In 2020 but not 2019:", paste(setdiff(codes_2020, codes_2019), collapse=", "), "\n")
}

# Create a matrix showing presence/absence across years
all_codes <- sort(unique(c(codes_2018, codes_2019, codes_2020)))
presence_matrix <- data.frame(
  Code = all_codes,
  Y2018 = all_codes %in% codes_2018,
  Y2019 = all_codes %in% codes_2019,
  Y2020 = all_codes %in% codes_2020
)

# Show any codes that aren't present in all years
inconsistent_codes <- presence_matrix[!(presence_matrix$Y2018 & presence_matrix$Y2019 & presence_matrix$Y2020), ]
if(nrow(inconsistent_codes) > 0) {
  cat("\nCodes not present in all years:\n")
  print(inconsistent_codes)
}