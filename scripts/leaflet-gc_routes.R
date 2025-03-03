### Leaflet map with sp great circle routes ###

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(htmltools)

# Load letters and geographic data
letters <- read_csv("data-raw/letters.csv", col_types = list(
  date = col_date("%Y%m%d"),
  rec_date = col_date("%Y%m%d"),
  resp_date = col_date("%Y%m%d")
))
locations <- read_csv("data/locations-geo.csv")
gcircles_routes <- read_sf("data/routes-sf.geojson")

# Filter by date if desired
# letters <- letters |> 
#   filter(year < 1592)

# Prepare the data to be joined together
# Calculate number of letters for each variable
# Rename location columns to placename so they can be joined together
# Name count column after variable they represent
# Thus, source goes from being a placename to a count etc.
per_route <- letters |> 
  count(source, destination) |> 
  drop_na() |> 
  arrange(n)

per_source <- letters |>
  rename(placename = source) |> 
  group_by(placename) |>
  summarise(source = n(),
            correspondents = n_distinct(writer)) |>
  drop_na()

per_destination <- letters |>
  rename(placename = destination) |> 
  group_by(placename) |>
  summarise(destination = n()) |>
  drop_na()

# Make above into geo data

routes <- per_route |> 
  left_join(gcircles_routes, by = join_by(source, destination)) |> 
  st_as_sf()

# Join the three above data frames together and join with location data
cities <- full_join(per_source, per_destination, by = "placename") |> # keep all items in both tables
  left_join(locations, by = "placename") |> 
  replace_na(list(source = 0, destination = 0, correspondents = 0))
    # replace NAs with 0s in count columns

# Filter cities data frame to have separate data frames with sources and destinations
# Both data frames have all information for the locations
sources <- filter(cities, source > 0)
destinations <- filter(cities, destination > 0)

# Color palette
pal <- colorNumeric(palette = "viridis", domain = routes$n, reverse = TRUE)

# Labels: make separate labels for sources and destinations to avoid mismatches
label1 <- sprintf(
  "%s to %s<br/>Distance: %g miles<br/>Letters: %g",
  routes$source, routes$destination, routes$miles, routes$n
) |> lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  destinations$placename, destinations$source, destinations$destination, destinations$correspondents
) |> lapply(htmltools::HTML)

label3 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  sources$placename, sources$source, sources$destination, sources$correspondents
) |> lapply(htmltools::HTML)

# Plot
leaflet(routes) |> 
  addProviderTiles(providers$CartoDB.PositronNoLabels) |>
  addCircleMarkers(data = destinations, lng = ~lng, lat = ~lat,
      color = "#31a354", stroke = FALSE, fillOpacity = 1, radius = 8, group = "Destinations",
      label = label2,
      labelOptions = labelOptions(textsize = "11px")) |>
  addCircleMarkers(data = sources, lng = ~lng, lat = ~lat,
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
