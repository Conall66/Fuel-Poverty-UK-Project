library(dplyr)
library(readr)

# Function to extract E06-E09 codes from a file
get_codes <- function(file_path) {
  data <- read_csv(file_path)
  
  # Handle different column names across years
  code_col <- if("LA.Code" %in% names(data)) "LA.Code" else "Area.Codes"
  
  codes <- sort(unique(data[[code_col]][grep("^E0[6-9]", data[[code_col]])]))
  return(codes)
}

# Get codes from all years
codes_list <- list()
for(year in 2011:2020) {
  filename <- paste0("Sub_Reg_Data_", year, "_Updated.csv")
  codes_list[[as.character(year)]] <- get_codes(filename)
}

# Compare number of codes across years
cat("Number of E06-E09 codes in each year:\n")
for(year in names(codes_list)) {
  cat(year, ": ", length(codes_list[[year]]), "\n")
}

# Check if all years have identical codes
first_year_codes <- codes_list[["2011"]]
all_identical <- TRUE
differences <- list()

for(year in names(codes_list)) {
  if(!identical(codes_list[[year]], first_year_codes)) {
    all_identical <- FALSE
    differences[[year]] <- list(
      missing = setdiff(first_year_codes, codes_list[[year]]),
      extra = setdiff(codes_list[[year]], first_year_codes)
    )
  }
}

cat("\nDo all years have identical codes? ", all_identical, "\n")

if(!all_identical) {
  cat("\nDifferences found:\n")
  for(year in names(differences)) {
    if(length(differences[[year]]$missing) > 0 || length(differences[[year]]$extra) > 0) {
      cat("\nYear", year, "differences:")
      if(length(differences[[year]]$missing) > 0) {
        cat("\n  Missing codes:", paste(differences[[year]]$missing, collapse=", "))
      }
      if(length(differences[[year]]$extra) > 0) {
        cat("\n  Extra codes:", paste(differences[[year]]$extra, collapse=", "))
      }
    }
  }
}

# Create presence/absence matrix
all_codes <- sort(unique(unlist(codes_list)))
presence_matrix <- matrix(FALSE, nrow=length(all_codes), ncol=length(codes_list))
colnames(presence_matrix) <- names(codes_list)
rownames(presence_matrix) <- all_codes

for(year in names(codes_list)) {
  presence_matrix[codes_list[[year]], year] <- TRUE
}

# Convert to data frame for easier viewing
presence_df <- as.data.frame(presence_matrix)
presence_df$Code <- rownames(presence_df)
presence_df <- presence_df[, c("Code", names(codes_list))]

# Show any inconsistent codes
inconsistent_codes <- presence_df[!apply(presence_df[,-1], 1, all), ]
if(nrow(inconsistent_codes) > 0) {
  cat("\nCodes not present in all years:\n")
  print(inconsistent_codes)
} else {
  cat("\nAll codes are consistent across all years!\n")
}

# Summary statistics
cat("\nSummary:\n")
cat("Total unique codes across all years:", length(all_codes), "\n")
cat("Number of codes consistent across all years:", 
    sum(apply(presence_matrix, 1, all)), "\n")library(dplyr)
library(readr)

# Function to extract E06-E09 codes from a file
get_codes <- function(file_path) {
  data <- read_csv(file_path)
  
  # Handle different column names across years
  code_col <- if("LA.Code" %in% names(data)) "LA.Code" else "Area.Codes"
  
  codes <- sort(unique(data[[code_col]][grep("^E0[6-9]", data[[code_col]])]))
  return(codes)
}

# Get codes from all years
codes_list <- list()
for(year in 2011:2020) {
  filename <- paste0("Sub_Reg_Data_", year, "_Updated.csv")
  codes_list[[as.character(year)]] <- get_codes(filename)
}

# Compare number of codes across years
cat("Number of E06-E09 codes in each year:\n")
for(year in names(codes_list)) {
  cat(year, ": ", length(codes_list[[year]]), "\n")
}

# Check if all years have identical codes
first_year_codes <- codes_list[["2011"]]
all_identical <- TRUE
differences <- list()

for(year in names(codes_list)) {
  if(!identical(codes_list[[year]], first_year_codes)) {
    all_identical <- FALSE
    differences[[year]] <- list(
      missing = setdiff(first_year_codes, codes_list[[year]]),
      extra = setdiff(codes_list[[year]], first_year_codes)
    )
  }
}

cat("\nDo all years have identical codes? ", all_identical, "\n")

if(!all_identical) {
  cat("\nDifferences found:\n")
  for(year in names(differences)) {
    if(length(differences[[year]]$missing) > 0 || length(differences[[year]]$extra) > 0) {
      cat("\nYear", year, "differences:")
      if(length(differences[[year]]$missing) > 0) {
        cat("\n  Missing codes:", paste(differences[[year]]$missing, collapse=", "))
      }
      if(length(differences[[year]]$extra) > 0) {
        cat("\n  Extra codes:", paste(differences[[year]]$extra, collapse=", "))
      }
    }
  }
}

# Create presence/absence matrix
all_codes <- sort(unique(unlist(codes_list)))
presence_matrix <- matrix(FALSE, nrow=length(all_codes), ncol=length(codes_list))
colnames(presence_matrix) <- names(codes_list)
rownames(presence_matrix) <- all_codes

for(year in names(codes_list)) {
  presence_matrix[codes_list[[year]], year] <- TRUE
}

# Convert to data frame for easier viewing
presence_df <- as.data.frame(presence_matrix)
presence_df$Code <- rownames(presence_df)
presence_df <- presence_df[, c("Code", names(codes_list))]

# Show any inconsistent codes
inconsistent_codes <- presence_df[!apply(presence_df[,-1], 1, all), ]
if(nrow(inconsistent_codes) > 0) {
  cat("\nCodes not present in all years:\n")
  print(inconsistent_codes)
} else {
  cat("\nAll codes are consistent across all years!\n")
}

# Summary statistics
cat("\nSummary:\n")
cat("Total unique codes across all years:", length(all_codes), "\n")
cat("Number of codes consistent across all years:", 
    sum(apply(presence_matrix, 1, all)), "\n")