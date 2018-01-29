### Create sf linestring object for routes ###

# Creates matrices for coordinates of sources and destinations Uses a for loop
# to turn this into list of matrices with source and destination and converts to
# sf_LINESTRING. Goes through process of list of sfg objects, to sfc to sf
# objects. Creates great circles with st_segmentize() when creating sfc object

library(tidyverse)
library(sf)

letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables

## Routes and create id column
routes <- letters %>%
  group_by(source, destination) %>% 
  summarise() %>% 
  remove_missing() %>% 
  ungroup()

routes <- add_column(routes, id = 1:nrow(routes))

## Create matrix of longitude and latitude for sources and destinations
sources <- routes %>% 
  left_join(locations, by = c("source" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

destinations <- routes %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

## Create sfg linestring objects in a list
linestrings_sfg <- vector("list", nrow(sources))
for (i in 1:nrow(sources)) {
  linestrings_sfg[[i]] <- st_linestring(rbind(sources[i, ], destinations[i, ]))
}

# Create sfc linestring from list
# Create great circles
linestrings_sfc <- st_sfc(linestrings_sfg, crs = 4326) %>% 
  st_segmentize(units::set_units(20, km))

# Create id tibble and join with sfc object to create sf object
linestrings_sf <- tibble(id = 1:length(linestrings_sfc)) %>% 
  st_sf(geometry = linestrings_sfc)

# Join with original routes tibble to add back information
routes_sf <- left_join(linestrings_sf, routes, by = "id")
