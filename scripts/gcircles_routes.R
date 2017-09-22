### Make great circles for routes:
### Export data as Spatial Lines Data Frame (sf) and as dataframe and tibble for ggplot ###

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables)

# Data from letters
per_route <- letters %>%
  group_by(source, destination) %>%
  filter(source != destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count) %>%
  ungroup()

# Join data to locations data and add id for each pair
geo_per_route <- per_route %>%
  left_join(locations, by = c("source" = "place")) %>% 
  left_join(locations, by = c("destination" = "place")) %>% 
  add_column(id = 1:nrow(per_route))

# Extract source and destination lat and lon data to be placed into gcIntermediate command
source_loc <- select(geo_per_route, lon.x, lat.x)
dest_loc <- select(geo_per_route, lon.y, lat.y)

# Calculate great circle routes between sources and destinations and return a SpatialLines object
routes <- gcIntermediate(source_loc, dest_loc, 100, addStartEnd = TRUE, sp = TRUE)

# Convert a SpatialLines object into SpatialLinesDataFrame, so that tabular data can be added

# create tibble with id column with length of routes
ids <- tibble(id = 1:nrow(geo_per_route))

# Convert SpatialLines into SpatialLinesDataFrame with id column

routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = TRUE)

### Join data as SpatialLinesDataFrame and save ###

gcircles_routes_sp <- sp::merge(routes, geo_per_route, by = "id")

write_rds(gcircles_routes_sp, "data/gcircles_routes_sp.rds")

### Tidy routes (convert to data frame) and join attributes ###
library(broom)

routes_df <- tidy(routes, region = "id") # convert SpatialLinesDataFrame to a dataframe so it can be used by ggplot
gcircles_routes_df <- left_join(routes_df, geo_per_route, by = "id")

write_rds(gcircles_routes_df, "data/gcircles_routes_df.rds")

## As tibble ##
gcircles_routes_tb <- as_tibble(gcircles_routes_df)

write_rds(gcircles_routes_tb, "gcircles_routes_tb.rds")
