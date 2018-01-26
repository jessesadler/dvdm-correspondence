## sp points

library(tidyverse)
library(sp)

locations <- read_csv("data/locations-1591.csv")

## Get coordinates: Make a matrix of the coordinates
coords <- locations %>% select(lon, lat)

# Make spatial points with proj4string
points_sp <- SpatialPoints(coords = coords, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Can also use EPSG code
# This results in slightly different name for the CRS,
# but the actual points are equivalent
points_epsg <- SpatialPoints(coords = coords, proj4string = CRS("+init=epsg:4326"))

# Get data from locations
data <- locations %>% select(place)

# Make spatial points data frame
locations_sp <- SpatialPointsDataFrame(coords = coords,
                                       data = data,  
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
