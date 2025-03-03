### Leaflet map with sp great circle routes ###

# Load libraries
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

# Load letters and geographic data
letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") |> 
  select(place:lat) # simplify locations data to only necessary variables
gcircles_routes <- read_sf("data/routes_sf.geojson")

# Prepare the data to be joined together
# Calculate number of letters for each variable
# Rename location columns to place so they can be joined together
# Name count column after variable they represent
# Thus, source goes from being a place to a count etc.
per_source <- letters |>
  rename(place = source) |> 
  group_by(place) |>
  summarise(source = n(),
            correspondents = n_distinct(writer)) |>
  drop_na()

per_destination <- letters |>
  rename(place = destination) |> 
  group_by(place) |>
  summarise(destination = n()) |>
  drop_na()

# Join the three above data frames together and join with location data
cities <- full_join(per_source, per_destination, by = "place") |> # keep all items in both tables
  left_join(locations, by = "place") |> 
  replace_na(list(source = 0, destination = 0, correspondents = 0))
    # replace NAs with 0s in count columns

# Filter cities data frame to have separate data frames with sources and destinations
# Both data frames have all information for the locations
sources <- filter(cities, source > 0)
destinations <- filter(cities, destination > 0)

# Color palette
pal <- colorNumeric(palette = "viridis", domain = gcircles_routes$n, reverse = TRUE)

# Labels: make separate labels for sources and destinations to avoid mismatches
label1 <- sprintf(
  "%s to %s<br/>Distance: %g miles<br/>Letters: %g",
  gcircles_routes$source, gcircles_routes$destination, gcircles_routes$miles, gcircles_routes$n
) |> lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  destinations$place, destinations$source, destinations$destination, destinations$correspondents
) |> lapply(htmltools::HTML)

label3 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  sources$place, sources$source, sources$destination, sources$correspondents
) |> lapply(htmltools::HTML)

# Plot
leaflet(gcircles_routes) |> 
  addProviderTiles(providers$CartoDB.PositronNoLabels) |>
  addCircleMarkers(data = destinations, lng = ~lon, lat = ~lat,
      color = "#31a354", stroke = FALSE, fillOpacity = 1, radius = 8, group = "Destinations",
      label = label2,
      labelOptions = labelOptions(textsize = "11px")) |>
  addCircleMarkers(data = sources, lng = ~lon, lat = ~lat,
      color = "#feb24c", stroke = FALSE, fillOpacity = 1, radius = 5, group = "Sources",
      label = label3,
      labelOptions = labelOptions(textsize = "11px")) |>
  addPolylines(opacity = 0.8, weight = 3, color = ~pal(n), group = "Routes",
      label = label1,
      labelOptions = labelOptions(textsize = "11px"),
      highlight = highlightOptions(weight = 5, color = "#c51b7d", opacity = 1)) |>
  addLegend(position = "topright",
      colors = c("#feb24c", "#31a354"),
      labels = c("Sent Location", "Received Location"),
      opacity = 1) |>
  addLegend(pal = pal, values = ~n, opacity = 1,
      title = "Letters<br/>Received") |> 
  addLayersControl(position = "bottomright",
      overlayGroups = c("Destinations", "Sources", "Routes"),
      options = layersControlOptions(collapsed = FALSE))
