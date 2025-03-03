### Geocode correspondence data ###

# Geocode locations in the letters tibble
# Add historical regions
# Take out country name from places that need it in letters data

# Load libraries
library(readr)
library(dplyr)
library(opencage)
library(sf)

letters <- read_csv("data-raw/letters.csv", col_types = list(
  date = col_date("%Y%m%d"),
  rec_date = col_date("%Y%m%d"),
  resp_date = col_date("%Y%m%d")
))

locations <- sort(unique(c(letters$source, letters$destination)))

geo <- oc_forward_df(locations,
              bounds = oc_bbox(-29, -10, 24, 60),
              output = "all")

# Het Vlie to Vlieland
vlie <- oc_forward_df("Vlieland, Netherlands", output = "all")
vlie[1,1] <- "Het Vlie"

geo <- geo |> 
  filter(placename != "Het Vlie") |> 
  bind_rows(vlie)

geo |> 
  select(placename:oc_lng, oc_formatted, oc_country, oc_state) |> 
  sf::st_as_sf(coords = c("oc_lng", "oc_lat"), crs = 4326) |> 
  mapview::mapview()

# Liege is wrong
liege <- oc_forward_df("Liege", output = "all", limit = 3)
liege <- liege |> 
  filter(oc_type == "city")

geo <- geo |> 
  filter(placename != "Liege") |> 
  bind_rows(liege)

# check
geo |> 
  select(placename:oc_lng, oc_formatted, oc_country, oc_state) |> 
  sf::st_as_sf(coords = c("oc_lng", "oc_lat"), crs = 4326) |> 
  mapview::mapview()

# Select columns
geo <- geo |> 
  select(placename, lat = oc_lat, lng = oc_lng, oc_formatted, oc_country, oc_state)

# Add historic regions

geo <- geo |> 
  mutate(historic_region = case_when(
    oc_country == "Belgium" ~ "Spanish Netherlands",
    oc_country == "Czechia" ~ "Holy Roman Empire",
    oc_country == "France" ~ "France",
    oc_country == "Gabon" ~ "Africa",
    oc_country == "Germany" ~ "Holy Roman Empire",
    oc_country == "Ghana" ~ "Africa",
    oc_country == "Italy" ~ "Italy",
    oc_country == "Morocco" ~ "Africa",
    oc_country == "Netherlands" ~ "Dutch Republic",
    oc_country == "Sweden" ~ "Sweden",
    oc_country == "United Kingdom" ~ "England",
  ))


# Create sf locations
geo_sf <- geo |> 
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)

# Save data
write_csv(geo, "data/locations-geo.csv")
st_write(geo_sf, "data/locations-sf.geojson")
