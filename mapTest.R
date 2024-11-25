install.packages("sf")
library(sf)
library(ggplot2)
library(dplyr)

la_boundaries <- st_read("Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102")
mortality_data <- read.csv("mortality_data.csv")

# Select just 2011/2012 data
mortality_data_2011 <- mortality_data %>%
  select(`Area.code`, `Area.name`, Winter.mortality.index..2011.2012)

# Then try joining with actual column names
uk_boundaries_with_data <- la_boundaries %>%
  left_join(mortality_data_2011, by = c("LAD24CD" = "Area.code"))


# Plot with standardized values for better color spread
gg <- ggplot() + 
  geom_sf(data = uk_boundaries_with_data, 
          aes(fill = scale(`Winter.mortality.index..2011.2012`)), 
          color = "white", 
          size = 0.25) +
  scale_fill_gradient2(low = "blue", mid = "red", high = "yellow", 
                       na.value = "white") +
  theme_void()


# To see the data structure:
head(landarea_boundaries)
ggsave("map.png", gg)

