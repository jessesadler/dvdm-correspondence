### Create sf linestring object for routes ###

# Use sf methods to create linestring for routes Key move is to create id column
# for groups and use gather to have all cities in one column. This makes it
# possible to have only one sf geometry column. Lines are made with
# group_by() and summarise(), along with st_cast() to create linestring

library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(sf)
library(units)

letters <- read_csv("data-raw/letters.csv", col_types = list(
  date = col_date("%Y%m%d"),
  rec_date = col_date("%Y%m%d"),
  resp_date = col_date("%Y%m%d")
))

# Use of csv locations makes final data frame nicer
locations <- read_csv("data/locations-geo.csv")

## Routes with id column
routes <- letters |> 
  group_by(source, destination) |> 
  summarise(.groups = "drop") |> 
  drop_na() |> 
  rowid_to_column("id")

## Gather to make long tibble go from source and destination as variables to
# place and whether it is source or destination. This makes it so there is only
# one set of longitude and latitude columns and so only one sfc column
routes_long <- routes |> 
  pivot_longer(cols = c(source, destination),
               names_to = "type",
               values_to = "place")

# Create sf points
sf_points <- routes_long |> 
  left_join(locations, by = join_by(place == placename)) |> 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

# Create sf great circle lines
routes_sf <- sf_points |> 
  group_by(id) |>  
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING") |> 
  st_segmentize(units::set_units(20, km))

# Add back in source and destination columns with city names
routes_sf <- routes_sf |> 
  left_join(routes, by = "id") |> 
  select(id, source, destination, geometry) # Rearrange columns

# Distance
routes_sf <- routes_sf |> 
  mutate(meters = st_length(geometry),
         miles = round(units::set_units(meters, miles)),
         .after = destination)


# Map routes by distance
mapview::mapview(routes_sf, zcol = "miles", legend = TRUE)

# Write out data
write_sf(routes_sf, "data/routes-sf.geojson", delete_dsn = TRUE)
