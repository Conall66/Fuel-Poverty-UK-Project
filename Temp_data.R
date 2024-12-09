
# Temperature data

library(janitor)

txt_file <- "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/datasets/Tmin/date/UK.txt"
data <- read.table(txt_file, skip = 7, header = TRUE, sep = "", stringsAsFactors = FALSE, fill = TRUE)

# Remove the first row
data <- data[-1, ]

# Keep only the first and the fifth last columns
data <- data[, c(1, ncol(data) - 4)]
colnames(data) <- c("Year", "Min_Temp")

# Filter rows for years 2012 to 2021
data <- data[data$Year >= 2012 & data$Year <= 2021, ]

file_name <- "UK_Temp.csv"
write.csv(data, file_name, row.names = FALSE)