library(dplyr)
library(readr)

# Read the 2011 data, properly skipping the header rows
data_2011 <- read_csv("Data Cleaning Stages/Sub_Reg_First_Pass/Sub_Reg_Data_2011_.csv", 
                      skip = 3,  # Skip both header rows
                      col_names = c("row", "LA Code", "LA Name", "Region", 
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor"))

# Read the 2012 data, properly skipping the header rows
data_2012 <- read_csv("Data Cleaning Stages/Sub_Reg_First_Pass/Sub_Reg_Data_2012_.csv",
                      skip = 3,  # Skip both header rows
                      col_names = c("row", "LA Code", "LA Name", "Region", 
                                    "Number of households", 
                                    "Number of households in fuel poverty",
                                    "Proportion of households fuel poor"))

# Create a lookup from 2012 names to E-codes
name_to_code_lookup <- data_2012 %>%
  select(`LA Name`, `LA Code`) %>%
  filter(!is.na(`LA Code`))  # Remove any NA rows

# Map the codes to 2011 data
standardized_2011 <- data_2011 %>%
  left_join(name_to_code_lookup, by = "LA Name") %>%
  rename(
    "Original_Code" = "LA Code.x",
    "Area Codes" = "LA Code.y"
  ) %>%
  select(
    "Area Codes",
    households = "Number of households",
    fuel_poor = "Number of households in fuel poverty",
    proportion = "Proportion of households fuel poor"
  ) %>%
  # Convert proportion to numeric and multiply by 100 if it's in decimal form
  mutate(proportion = as.numeric(proportion) * 100)

# Check for any unmatched areas
unmatched_areas <- data_2011 %>%
  filter(!`LA Name` %in% name_to_code_lookup$`LA Name`) %>%
  select(`LA Name`)

cat("Unmatched areas from 2011:\n")
print(unmatched_areas)

# Print sample of matched data
cat("\nSample of standardized 2011 data:\n")
print(head(standardized_2011))

# Save the standardized data
write_csv(standardized_2011, "Sub_Reg_2011_NewLACodes")