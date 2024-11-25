install.packages("sf")

library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(sf)

# Or using relative path if not using here package
la_boundaries <- st_read("Local_Authority_Districts_May_2024_Boundaries_UK_BGC_-5850961694214429102")


