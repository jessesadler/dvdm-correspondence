### Figure out distance along great circle routes with distGeo function from geoSphere package ###

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables

# Data from letters
routes <- letters %>%
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  ungroup()

# Join data to locations data and add id for each pair
geo_per_route <- routes %>%
  left_join(locations, by = c("source" = "place")) %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  rowid_to_column("id")

# Extract source and destination lat and lon data to be placed into distance command
source_loc <- select(geo_per_route, lon.x, lat.x)
dest_loc <- select(geo_per_route, lon.y, lat.y)

# Calculate distance along great circle routes and return a vector of distance in meters
distance <- distGeo(source_loc, dest_loc)

# Turn vector into a tibble, rename value column to meters, and create kilometers, miles, and id columns
distance <- as_tibble(distance) %>%
  rename(meters = value) %>% 
  mutate(kms = meters/1000) %>% 
  mutate(miles = meters/1609) %>% 
  rowid_to_column("id")

routes_distance <- full_join(geo_per_route, distance, by = "id")
