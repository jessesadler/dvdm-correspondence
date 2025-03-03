### sf points ###

# This script takes the locations with longitude and latitude and
# creates an sf points object. This is then combined with the letters
# data to get aggregates for source and destination

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

locations <- read_csv("data/locations-1591.csv")

### Convert locations data frame to sf_points object ###
# This is the line that does the actual conversion.
# coords is self explanatory; crs adds a coordinate system;
# agr is short for  attribute-geometry-relationship,
# specifies for each non-geometry attribute column how it relates to the geometry.
# "constant" is used for attributes that are constant throughout the geometry

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
# Can also use crs = "+proj=longlat +datum=WGS84"

# Save data
write_rds(locations_sf, "data/locations_sf.rds")

### Map locations ###
mapview(locations_sf)

leaflet(locations_sf) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers()

### Get letters per source and letters per destination ###
# Join this data to locations_sf to make it an sfc object.
# Need separate source and destination sf objects to
# distinguish which colors to plot in leaflet map.
# With leaflet map, cities sf object still necessary
# to give info for labels.

letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations_sf <- select(locations_sf, place, geometry) # simplify locations data

per_source <- letters %>%
  group_by(source) %>%
  rename(place = source) %>% 
  summarise(source = n(),
            correspondents = n_distinct(writer)) %>% 
  drop_na()

# Can either use left_join with drop_na() or right_join
source_sf <- left_join(locations_sf, per_source, by = "place") %>% 
  drop_na()

per_destination <- letters %>%
  group_by(destination) %>%
  rename(place = destination) %>% 
  summarise(destination = n()) %>% 
  drop_na()

# Use right_join
destination_sf <- right_join(locations_sf, per_destination, by = "place")

cities <- full_join(per_source, per_destination, by = "place") %>% 
  replace_na(list(source = 0, destination = 0, correspondents = 0))
cities_sf <- left_join(locations_sf, cities, by = "place")

mapview(cities_sf)

# Leaflet
label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  cities_sf$place, cities_sf$source, cities_sf$destination, cities_sf$correspondents
) %>% lapply(htmltools::HTML)

leaflet() %>% addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(data = destination_sf,
                   color = "#addd8e", stroke = FALSE, fillOpacity = 1, radius = 8, 
                   group = "Destinations",
                   label = label2,
                   labelOptions = labelOptions(textsize = "11px")) %>%
  addCircleMarkers(data = source_sf,
                   color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 5, 
                   group = "Sources",
                   label = label2,
                   labelOptions = labelOptions(textsize = "11px"))
