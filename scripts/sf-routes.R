### Create sf linestring object for routes ###

# Create sfc linestring object from sp gcIntermediate
# Add routes id column to letters and
# add distance data to the routes data.
# This sets up and saves sf data for routes

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)
library(sf)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables)

# Get tibble of the routes in the data
routes <- letters %>%
  group_by(source, destination) %>% 
  summarise() %>% 
  remove_missing()

# Create tibble with id column with length of routes
# Used for routes and letters tibbles and spatial lines data frame
ids <- tibble(id = 1:nrow(routes))

routes <- bind_cols(ids, routes)

# Add route ids to letters data frame
# Joine by two variables
letters_id <- left_join(letters, routes, by = c("source" = "source", "destination" = "destination"))

# Geo data
geo_routes <- routes %>%
  left_join(locations, by = c("source" = "place")) %>% 
  left_join(locations, by = c("destination" = "place"))

source_loc <- select(geo_routes, lon.x, lat.x)
dest_loc <- select(geo_routes, lon.y, lat.y)

# Calculate great circle routes between sources and destinations
# and return a SpatialLines object
gc_routes <- gcIntermediate(source_loc, dest_loc, 100, addStartEnd = TRUE, sp = TRUE)

# Convert SpatialLines into SpatialLinesDataFrame with id column
gc_routes <- SpatialLinesDataFrame(gc_routes, data = ids, match.ID = TRUE)

# Join data as SpatialLinesDataFrame and then convert to sf
routes_sp <- sp::merge(gc_routes, routes, by = "id")

routes_sf <- st_as_sf(routes_sp)

### Distance ###
# Add distance data to routes_sf object

distance_vector <- st_length(routes_sf) %>% as.integer()
# Use as.integer to change vector from class units to integer
# Could use as.numeric to get more accurate
# Whole numbers probably better since I do not have exact location

distance <- as_tibble(distance_vector) %>%
  rename(meters = value) %>% 
  mutate(kms = round(meters/1000)) %>% 
  mutate(miles = round(meters/1609)) %>% 
  add_column(id = 1:nrow(routes))

# Distance with routes information to have names of cities
# This creates distance tibble by itself
distance_tbl <- left_join(routes, distance, by = "id")

# Add distance data to routes_sf data
routes_sf <- left_join(routes_sf, distance, by = "id")

### Save data ###
write_rds(routes_sf, "data/routes_sf.rds")
write_csv(letters_id, "data/letters_id.csv")

### Mapping ###
library(leaflet)
library(mapview)

mapview(routes_sf)

leaflet(routes_sf) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  addPolylines()

### Add count data ###
# This shows how to add number of letters for each route
# Example is given for dealing with a subset by time.
# The key is to do filtering on letters data and then
# join it to routes_sf object

per_route <- letters_id %>%
  group_by(id) %>%
  summarise(count = n()) %>%
  remove_missing()

count_sf <- left_join(routes_sf, per_route, by = "id") %>% 
  arrange(count)

# Subset of data
in_1585 <- letters_id %>%
  filter(year == 1585) %>% 
  group_by(id) %>%
  summarise(count = n()) %>%
  remove_missing()

# Either use right join or left join and remove NA
sf_1585 <- right_join(routes_sf, in_1585, by = "id") %>% 
  arrange(count)

sf_1585 <- left_join(routes_sf, in_1585, by = "id") %>% 
  arrange(count) %>% 
  remove_missing()

# Leaflet
pal <- colorNumeric(palette = "viridis", domain = count_sf$count, reverse = TRUE)

label1 <- sprintf(
  "%s to %s<br/>Letters: %g",
  count_sf$source, count_sf$destination, count_sf$count
) %>% lapply(htmltools::HTML)

leaflet(count_sf) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolylines(opacity = 0.8, weight = 3, color = ~pal(count),
               label = label1,
               labelOptions = labelOptions(textsize = "11px"),
               highlight = highlightOptions(weight = 5, color = "#ff9600", opacity = 1))
