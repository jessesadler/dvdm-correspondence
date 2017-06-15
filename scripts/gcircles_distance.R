### Figure out distance along great circle routes with distGeo function from geoSphere package ###

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables

# Data from letters
per_route <- letters %>%
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count) %>%
  ungroup()

# Join data to locations data
geo_per_route <- per_route %>%
  left_join(geo_data, by = c("source" = "place")) %>% 
  left_join(geo_data, by = c("destination" = "place"))
geo_per_route$id <- as.character(c(1:nrow(geo_per_route))) # create id for each pair

# Extract source and destination lat and lon data to be placed into distance command
source_loc <- select(geo_per_route, lon.x, lat.x)
dest_loc <- select(geo_per_route, lon.y, lat.y)

# Calculate distance along great circle routes and return a vector of distance in meters
distance <- distGeo(source_loc, dest_loc)

# Turn vector into a tibble, rename value column to meters and create kilometers column
distance <- as_tibble(distance) %>%
  rename(meters = value) %>% 
  mutate(kms = meters/1000)

# Add id row to be able to merge with route data
distance$id <- as.character(c(1:nrow(distance)))

routes_distance <- full_join(geo_per_route, distance, by = "id")
