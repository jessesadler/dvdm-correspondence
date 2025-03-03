### Create sf linestring object for routes ###

# Creates matrices for coordinates of sources and destinations Uses a for loop
# to turn this into list of matrices with source and destination and converts to
# sf_LINESTRING. Goes through process of list of sfg objects, to sfc to sf
# objects. Creates great circles with st_segmentize() when creating sfc object

library(tidyverse)
library(sf)

letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables

## Routes and create id column
routes <- letters %>%
  group_by(source, destination) %>% 
  summarise() %>% 
  drop_na() %>% 
  ungroup()%>% 
  rowid_to_column("id")

## Create matrix of longitude and latitude for sources and destinations
sources <- routes %>% 
  left_join(locations, by = c("source" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

destinations <- routes %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  select(lon:lat) %>% 
  as.matrix()

# Create sfg linestring objects in a list
# Access individual sfg objects with [[]] notation
linestrings_sfg <- vector("list", nrow(routes))

for (i in 1:nrow(routes)) {
  linestrings_sfg[[i]] <- st_linestring(rbind(sources[i, ], destinations[i, ]))
}

# Create sfc linestring from list, add CRS, and
# create great circles with st_sementize
linestrings_sfc <- st_sfc(linestrings_sfg, crs = 4326) %>% 
  st_segmentize(units::set_units(20, km))

# Join sfc object with original routes tibble to create sf object
routes_sf <- st_sf(routes, geometry = linestrings_sfc)
