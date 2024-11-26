# Read the CSV data
data_2018 <- read.csv("Sub_Reg_Extrcsv/Sub_Reg_Data_2018_Transitions.csv")
data_2019 <- read.csv("Sub_Reg_Extrcsv/Sub_Reg_Data_2019_Transitions.csv")
data_2020 <- read.csv("Sub_Reg_Extrcsv/Sub_Reg_Data_2020_Transitions.csv")
lad_data <- read.csv("Local_Authority_Districts_December_2022_UK_BGC_V2_-7833039764417853105.csv")

# Function to filter E06-E09 codes
filter_e_codes <- function(codes) {
  codes[grep("^E0[6-9]", codes)]
}

# Get unique LA codes from each dataset
lad_codes <- unique(filter_e_codes(lad_data$LAD22CD))
codes_2018 <- unique(filter_e_codes(data_2018$LA.Code))
codes_2019 <- unique(filter_e_codes(data_2019$Area.Codes))
codes_2020 <- unique(filter_e_codes(data_2020$Area.Codes))

# Find missing and extra codes using set operations
missing_in_2018 <- setdiff(lad_codes, codes_2018)
missing_in_2019 <- setdiff(lad_codes, codes_2019)
missing_in_2020 <- setdiff(lad_codes, codes_2020)

extra_in_2018 <- setdiff(codes_2018, lad_codes)
extra_in_2019 <- setdiff(codes_2019, lad_codes)
extra_in_2020 <- setdiff(codes_2020, lad_codes)

# Print analysis results
cat("Cross-Reference Analysis Results\n\n")
cat("1. Total number of E06-E09 codes in each dataset:\n")
cat(sprintf("Local Authority Districts: %d\n", length(lad_codes)))
cat(sprintf("2018 Transitions: %d\n", length(codes_2018)))
cat(sprintf("2019 Transitions: %d\n", length(codes_2019)))
cat(sprintf("2020 Transitions: %d\n\n", length(codes_2020)))

cat("2. Missing codes (in LAD but not in Transitions):\n")
cat("\n2018 Missing Codes:\n")
print(sort(missing_in_2018))
cat("\n2019 Missing Codes:\n")
print(sort(missing_in_2019))
cat("\n2020 Missing Codes:\n")
print(sort(missing_in_2020))

cat("\n3. Extra codes (in Transitions but not in LAD):\n")
cat("\n2018 Extra Codes:\n")
print(sort(extra_in_2018))
cat("\n2019 Extra Codes:\n")
print(sort(extra_in_2019))
cat("\n2020 Extra Codes:\n")
print(sort(extra_in_2020))

# Create summary dataframe of matches/mismatches
summary_df <- data.frame(
  Year = c("2018", "2019", "2020"),
  Total_LAD_Codes = length(lad_codes),
  Total_Transition_Codes = c(length(codes_2018), length(codes_2019), length(codes_2020)),
  Missing_Codes = c(length(missing_in_2018), length(missing_in_2019), length(missing_in_2020)),
  Extra_Codes = c(length(extra_in_2018), length(extra_in_2019), length(extra_in_2020))
)

# Print summary table
cat("\n4. Summary Table:\n")
print(summary_df)