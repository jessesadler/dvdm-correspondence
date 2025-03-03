### Create sf linestring object for routes ###

# Use sf methods to create linestring for routes Key move is to create id column
# for groups and use gather to have all cities in one column. This makes it
# possible to have only one sf geometry column. Lines are made with
# group_by() and summarise(), along with st_cast() to create linestring

library(tidyverse)
library(sf)
library(units)

letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables

## Routes and create id column
routes <- letters |> 
  count(source, destination) |>  
  drop_na() |>  
  rowid_to_column("id")

## Gather to make long tibble go from source and destination as variables to
# place and whether it is source or destination. This makes it so there is only
# one set of longitude and latitude columns and so only one sfc column
routes_long <- routes |> 
  pivot_longer(cols = c(source, destination), names_to = "type", values_to = "place")


# Add latitude and longitude data
sf_points <- routes_long |> 
  left_join(locations, by = "place") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Create sf great circles
routes_sf <- sf_points |> 
  group_by(id) |>  
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING") |> 
  st_segmentize(units::set_units(20, km))

# Add back in source and destination columns with city names
routes_sf <- routes_sf |> 
  left_join(routes, by = "id") |> 
  select(id, source, destination, n, geometry) # Rearrange columns

# Distance
routes_sf <- routes_sf |> 
  mutate(meters = st_length(geometry),
         miles = round(units::set_units(meters, miles)),
         .after = n)


### Plot
library(mapview)

# Map routes by distance
mapview(routes_sf, zcol = "miles", legend = TRUE)
mapview(routes_sf, zcol = "n", legend = TRUE)

# Write out data
write_sf(routes_sf, "data/routes_sf.geojson")
