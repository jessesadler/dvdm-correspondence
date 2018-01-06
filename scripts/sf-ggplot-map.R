## sf map with points and routes in ggplot2 ##

# This script goes through the whole process of
# making sf points, sf routes, and sf background map.
# It then plots these using ggplot2.
# Source and destination points are made in two different ways:
# as one sf tibble and as separate sf tibbles

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rmapshaper)
library(ggrepel)

### Data ###
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat)

### Set up points data ###

sources <- letters %>% 
  group_by(source) %>% 
  count() %>% 
  rename(place = source) %>% 
  add_column(type = "source") %>% 
  remove_missing()

destinations <- letters %>% 
  group_by(destination) %>% 
  count() %>% 
  rename(place = destination) %>% 
  add_column(type = "destination") %>% 
  remove_missing()

# Use full join to glue the two tibbles together
points <- full_join(sources, destinations)
points_geo <- left_join(points, locations, by = "place")

### sf points ###
# There is not really much reason to use points_sf
# in the current set up
points_sf <- st_as_sf(points_geo, coords = c("lon", "lat"), crs = 4326)

### Alternative to get separate sources_sf and destinations_sf ###
sources_sf <- sources %>%
  left_join(locations, by = "place") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

destinations_sf <- destinations %>% 
  left_join(locations, by = "place") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

### Routes ###
# This is from sf_linestring script with small changes

# Create routes data with id column
# This will be used to make linestring
# and will be added back in to have linestring with the data
routes <- letters %>%
  group_by(source, destination) %>% 
  count() %>% 
  remove_missing() %>% 
  arrange(n)

routes <- add_column(routes, id = 1:nrow(routes))

# Gather to make long tibble
# Remove count data since it is not necessary
# and will be added later
routes_long <- routes %>% 
  select(-n) %>% 
  gather(type, place, -id)

# Add latitude and longitude data
routes_geo <- left_join(routes_long, locations, by = "place")

# Create sf object with sfc points
routes_points <- st_as_sf(routes_geo, coords = c("lon", "lat"), crs = 4326)

# Make lines through group_by and summarise
# do-union = FALSE makes multipoint geometry
# This is cast into linestring
# Then segmentized into great circles
routes_lines <- routes_points %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_segmentize(units::set_units(50, km))

# Add back in source and destination columns with city names
routes_sf <- left_join(routes_lines, routes, by = "id")

### Background map ###

# Find bounding box of points_sf
st_bbox(points_sf)

map_sf <- ne_coastline(scale = "medium", returnclass = "sf")
bbox <- c(-5, 38, 17, 55)

# Use rmapshaper to clip size of map
background_sf <- ms_clip(map_sf, bbox = bbox)

## Labels of most significant places ##
labels <- points_geo %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  distinct(place, .keep_all = TRUE) %>% 
  top_n(n = 5, wt = n)
  

### Plot map ###
# Neither of these plots are great, but they give an idea

# With points_sf

ggplot() + 
  geom_sf(data = background_sf) + 
  geom_sf(data = points_sf, aes(color = type, size = n), alpha = 0.8, show.legend = "point") + 
  geom_sf(data = routes_sf, size = 0.4, alpha = 0.8, color = "lightgray") + 
  geom_text_repel(data = labels, aes(x = lon, y = lat, label = place)) + 
  coord_sf(datum = NA) + 
  theme_void()

# With separate source and destination sf objects

ggplot() + 
  geom_sf(data = background_sf) + 
  geom_sf(data = destinations_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#1b9e77", show.legend = "point") + 
  geom_sf(data = sources_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#7570b3", show.legend = "point") + 
  geom_sf(data = routes_sf, size = 0.4, alpha = 0.8, aes(color = n)) + 
  scale_colour_viridis_c(direction = -1, option = "C") + 
  coord_sf(datum = NA) + 
  labs(size = "Letters", color = NULL) +
  theme_void()