# Read the 2019 data
data_2019 <- read.csv("Sub_Reg_extrcsv/Sub_Reg_Data_2019_Transitions.csv")

# Extract the rows for areas that merged into North Northamptonshire
north_areas <- c("E07000150", "E07000152", "E07000153", "E07000156")  # Corby, East Northants, Kettering, Wellingborough
north_data <- data_2019[data_2019$Area.Codes %in% north_areas, ]

# Extract the rows for areas that merged into West Northamptonshire
west_areas <- c("E07000151", "E07000154", "E07000155")  # Daventry, Northampton, South Northants
west_data <- data_2019[data_2019$Area.Codes %in% west_areas, ]

# Calculate new entries
north_northants <- data.frame(
  Area.Codes = "E06000061",
  households = sum(north_data$households),
  fuel_poor = sum(north_data$fuel_poor),
  proportion = (sum(north_data$fuel_poor) / sum(north_data$households)) * 100
)

west_northants <- data.frame(
  Area.Codes = "E06000062",
  households = sum(west_data$households),
  fuel_poor = sum(west_data$fuel_poor),
  proportion = (sum(west_data$fuel_poor) / sum(west_data$households)) * 100
)

# Remove old authorities and add new ones
data_2019_updated <- data_2019[!data_2019$Area.Codes %in% c(north_areas, west_areas), ]
data_2019_updated <- rbind(data_2019_updated, north_northants, west_northants)

# Sort by Area Codes
data_2019_updated <- data_2019_updated[order(data_2019_updated$Area.Codes), ]

# Print the old and new figures for verification
cat("North Northamptonshire (E06000061) created from:\n")
print(north_data[, c("Area.Codes", "households", "fuel_poor", "proportion")])
cat("\nResulting in new authority:\n")
print(north_northants)

cat("\nWest Northamptonshire (E06000062) created from:\n")
print(west_data[, c("Area.Codes", "households", "fuel_poor", "proportion")])
cat("\nResulting in new authority:\n")
print(west_northants)

# Verification of total households preserved
cat("\nVerification:\n")
cat("Original total households in affected areas:", 
    sum(north_data$households) + sum(west_data$households), "\n")
cat("New total households in replacement areas:", 
    north_northants$households + west_northants$households, "\n")


write.csv(data_2019_updated, 
          file = "Sub_Reg_Data_2019_Updated.csv")
