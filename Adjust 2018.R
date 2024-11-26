# Read the 2018 data
data_2018 <- read.csv("Sub_Reg_extrcsv/Sub_Reg_Data_2018_Transitions.csv")

# 1. NORTHAMPTONSHIRE REORGANIZATION
# North Northants areas
north_areas <- c("E07000150", "E07000152", "E07000153", "E07000156")
north_data <- data_2018[data_2018$LA.Code %in% north_areas, ]

# West Northants areas
west_areas <- c("E07000151", "E07000154", "E07000155")
west_data <- data_2018[data_2018$LA.Code %in% west_areas, ]

# Calculate new North and West Northants entries
north_northants <- data.frame(
  LA.Code = "E06000061",
  households = sum(north_data$households),
  fuel_poor = sum(north_data$fuel_poor),
  proportion = (sum(north_data$fuel_poor) / sum(north_data$households)) * 100
)

west_northants <- data.frame(
  LA.Code = "E06000062",
  households = sum(west_data$households),
  fuel_poor = sum(west_data$fuel_poor),
  proportion = (sum(west_data$fuel_poor) / sum(west_data$households)) * 100
)

# 2. BUCKINGHAMSHIRE REORGANIZATION
bucks_areas <- c("E07000004", "E07000005", "E07000006", "E07000007")
bucks_data <- data_2018[data_2018$LA.Code %in% bucks_areas, ]

# Calculate new Buckinghamshire entry
buckinghamshire <- data.frame(
  LA.Code = "E06000060",
  households = sum(bucks_data$households),
  fuel_poor = sum(bucks_data$fuel_poor),
  proportion = (sum(bucks_data$fuel_poor) / sum(bucks_data$households)) * 100
)

# 3. CODE CHANGES
code_changes <- c(
  "E06000048" = "E06000057",  # Northumberland
  "E07000100" = "E07000240",  # St Albans
  "E07000097" = "E07000242",  # East Hertfordshire
  "E07000104" = "E07000241",  # Welwyn Hatfield
  "E08000020" = "E08000037",  # Gateshead
  "E07000101" = "E07000243"   # Stevenage
)

# Remove old entries and add new ones
data_2018_updated <- data_2018[!data_2018$LA.Code %in% c(north_areas, west_areas, bucks_areas), ]
data_2018_updated <- rbind(data_2018_updated, north_northants, west_northants, buckinghamshire)

# Update the codes
for(old_code in names(code_changes)) {
  data_2018_updated$LA.Code[data_2018_updated$LA.Code == old_code] <- code_changes[old_code]
}

# Sort by LA Code
data_2018_updated <- data_2018_updated[order(data_2018_updated$LA.Code), ]

# Verification
cat("Verification of transformations:\n\n")

cat("1. Northamptonshire Reorganization:\n")
cat("North Northants (E06000061) created from", length(north_areas), "authorities\n")
cat("Total households:", north_northants$households, "\n")
cat("Fuel poor percentage:", round(north_northants$proportion, 2), "%\n\n")

cat("West Northants (E06000062) created from", length(west_areas), "authorities\n")
cat("Total households:", west_northants$households, "\n")
cat("Fuel poor percentage:", round(west_northants$proportion, 2), "%\n\n")

cat("2. Buckinghamshire Reorganization:\n")
cat("Buckinghamshire (E06000060) created from", length(bucks_areas), "authorities\n")
cat("Total households:", buckinghamshire$households, "\n")
cat("Fuel poor percentage:", round(buckinghamshire$proportion, 2), "%\n\n")

cat("3. Code Changes:\n")
for(old_code in names(code_changes)) {
  cat(sprintf("Changed %s to %s\n", old_code, code_changes[old_code]))
}

cat("\n4. Overall Changes:\n")
cat("Original number of authorities:", nrow(data_2018), "\n")
cat("Updated number of authorities:", nrow(data_2018_updated), "\n")

# Check for any remaining old codes
old_codes <- c(north_areas, west_areas, bucks_areas, names(code_changes))
remaining_old <- data_2018_updated$LA.Code[data_2018_updated$LA.Code %in% old_codes]
if(length(remaining_old) > 0) {
  cat("\nWARNING: Found remaining old codes:", paste(remaining_old, collapse=", "), "\n")
}





write.csv(data_2018_updated, 
          file = "Sub_Reg_Data_2018_Updated.csv")