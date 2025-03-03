### Create great circles with sp and geosphere ###

# Create SpatialLinesDataFrame object with geosphere::gcIntermediate
# Convert SPLDF to sf linestring with sf and data frame with broom

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables

# Routes and create id column
# id column not actually necessary to create SLDF 
routes <- letters %>%
  group_by(source, destination) %>% 
  summarise() %>% 
  drop_na() %>% 
  ungroup()%>% 
  rowid_to_column("id")

# Geo data for sources and destinations
# Can be either data frame or matrix
sources <- routes %>% 
  left_join(locations, by = c("source" = "place")) %>% 
  select(lon:lat)

destinations <- routes %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  select(lon:lat)

# Calculate great circle routes between sources and destinations
# and return a SpatialLines object with geosphere::gcIntermediate.
# This adds CRS of EPSG 4326.
gc_routes_sl <- gcIntermediate(sources, destinations, 50, addStartEnd = TRUE, sp = TRUE)

# Convert SpatialLines into SpatialLinesDataFrame by using data from routes.
# Default is match.ID = TRUE
gc_routes_sldf <- SpatialLinesDataFrame(gc_routes_sl, data = routes)

# Not run: Save SLDF object
# write_rds(gc_routes_sldf, "data/gcircles_routes_sp.rds")


### Conversion to sf and data frame ###
library(sf)
library(broom)

# Convert SLDF to sf Linestring
routes_sf <- st_as_sf(gc_routes_sldf)


# Tidy SLDF with broom
# Convert id to integer to be able to do join
routes_df <- tidy(gc_routes_sldf, region = "id") %>% 
  mutate(id = as.integer(id))s

# Join spatial data from SLDF with non-spatial data
gc_routes_df <- left_join(routes_df, routes, by = "id")
