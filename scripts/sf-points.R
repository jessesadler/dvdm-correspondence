### sf points ###

# This script takes the locations with longitude and latitude and
# creates an sf points object. This is then combined with the letters
# data to get aggregates for source and destination

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat)

### Convert locations data frame to sf_points object ###
# This is the line that does the actual conversion.
# coords is self explanatory; crs adds a coordinate system;
# agr is short for  attribute-geometry-relationship,
# specifies for each non-geometry attribute column how it relates to the geometry.
# "constant" is used for attributes that are constant throughout the geometry

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326, agr = "constant")

mapview(locations_sf)
leaflet(locations_sf) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers()

### Get letters per source and letters per destination ###
# Join this data to locations_sf to make it an sfc object

per_source <- letters %>%
  group_by(source) %>%
  rename(place = source) %>% 
  summarise(source = n()) %>%
  remove_missing()

per_destination <- letters %>%
  group_by(destination) %>%
  rename(place = destination) %>% 
  summarise(destination = n()) %>%
  remove_missing()

corrs_per <- letters %>%
  group_by(source) %>%
  summarise(correspondents = n_distinct(writer)) %>% 
  rename(place = source)

cities <- full_join(per_source, per_destination, by = "place") %>% 
  left_join(corrs_per, by = "place") %>% 
  replace_na(list(source = 0, destination = 0, correspondents = 0))

cities_sf <- left_join(locations_sf, cities, by = "place")
# Need to have sf object as left data frame to have result be sf rather than tibble

mapview(cities_sf)

# Leaflet
label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  cities_sf$place, cities_sf$source, cities_sf$destination, cities_sf$correspondents
) %>% lapply(htmltools::HTML)

leaflet(cities_sf) %>% addProviderTiles(providers$CartoDB) %>%
  addCircleMarkers(color = "#addd8e", stroke = FALSE, fillOpacity = 1, radius = 8, 
                   group = "Destinations",
                   label = label2,
                   labelOptions = labelOptions(textsize = "11px")) %>%
  addCircleMarkers(color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 5, 
                   group = "Sources",
                   label = label2,
                   labelOptions = labelOptions(textsize = "11px"))
