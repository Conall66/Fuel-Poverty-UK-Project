install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)

la_boundaries <- st_read("Local_Authority_Districts_December_2022_UK_BGC_V2_8602835163392188905")
mortality_data <- read.csv("mortality_data.csv")

# Select just 2011/2012 data
mortality_data_2017 <- mortality_data %>%
  select(`Area.code`, `Area.name`, Winter.mortality.index..2015.2016)

# Then try joining with actual column names
uk_boundaries_with_data <- la_boundaries %>%
  left_join(mortality_data_2017, by = c("LAD22CD" = "Area.code"))


# Plot with standardized values for better color spread
gg <- ggplot() + 
  geom_sf(data = uk_boundaries_with_data, 
          aes(fill = scale(`Winter.mortality.index..2015.2016`)), 
          color = "white", 
          size = 0.25) +
  scale_fill_gradient2(low = "blue", mid = "red", high = "yellow", 
                       na.value = "white") +
  theme_void()


# To see the data structure:
ggsave("map15-16.png", gg)

