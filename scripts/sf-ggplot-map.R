## sf map with points and routes in ggplot2 ##

# This script goes through the whole process of
# making sf points, sf routes, and sf background map.
# It then plots these using ggplot2.
# Source and destination points are made in two different ways:
# as one sf tibble and as separate sf tibbles

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)

### Data ###
letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat)

### Set up points data ###
sources <- letters %>% 
  group_by(source) %>% 
  count() %>% 
  rename(place = source) %>% 
  add_column(type = "source") %>% 
  drop_na()

destinations <- letters %>% 
  group_by(destination) %>% 
  count() %>% 
  rename(place = destination) %>% 
  add_column(type = "destination") %>% 
  drop_na()

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
  drop_na() %>% 
  ungroup()%>% 
  arrange(n) %>% 
  rowid_to_column("id")

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
# Two different background maps
# One that is just coastlines, creating multilinestring object
# The other is countries, creating miultipolygon object
# The second map can be filled in

coastline_sf <- ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent) # Simplify columns from the default 64 columns

## Labels of most significant places ##
labels <- points_geo %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  distinct(place, .keep_all = TRUE) %>% 
  top_n(n = 5, wt = n)
  
### Plot map ###

# Coastline map with points_sf

ggplot() + 
  geom_sf(data = coastline_sf) + 
  geom_sf(data = points_sf, aes(color = type, size = n), alpha = 0.8, show.legend = "point") + 
  geom_sf(data = routes_sf, size = 0.4, alpha = 0.8, color = "lightgray") + 
  geom_text_repel(data = labels, aes(x = lon, y = lat, label = place)) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) + 
  theme_void()

# Source and Destination points faceted

ggplot() + 
  geom_sf(data = coastline_sf) + 
  geom_sf(data = points_sf, aes(size = n, color = type), alpha = 0.8, show.legend = "point") + 
  facet_wrap(~ type) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) + 
  theme_void()

# With separate source and destination sf objects

ggplot() + 
  geom_sf(data = coastline_sf) + 
  geom_sf(data = destinations_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#1b9e77", show.legend = "point") + 
  geom_sf(data = sources_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#7570b3", show.legend = "point") + 
  geom_sf(data = routes_sf, size = 0.4, alpha = 0.8, aes(color = n)) + 
  viridis::scale_colour_viridis(direction = -1, option = "C") + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) + 
  labs(size = "Letters", color = NULL) +
  theme_void()


## Countries map ##
# This makes it possible to fill in the land color
# The easiest way to get rid of country borders is to
# have `color` and `fill` be the same color

ggplot() + 
  geom_sf(data = countries_sf, color = "gray", fill = "lightgray") + 
  geom_sf(data = destinations_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#1b9e77", show.legend = "point") + 
  geom_sf(data = sources_sf, 
          aes(size = n), 
          alpha = 0.8, color = "#7570b3", show.legend = "point") + 
  geom_sf(data = routes_sf, size = 0.4, alpha = 0.8, aes(color = n)) + 
  viridis::scale_colour_viridis(direction = -1, option = "C") + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) + 
  labs(size = "Letters", color = NULL) +
  theme_void()
