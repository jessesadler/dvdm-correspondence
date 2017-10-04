### Leaflet map with sp great circle routes ###

# Load libraries
library(tidyverse)
library(leaflet)
library(htmltools)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv") %>% 
  select(place:lat) # simplify locations data to only necessary variables
gcircles_routes <- read_rds("data/gcircles_routes_sp.rds")

# Prepare the data to be joined together
# Calculate number of letters for each variable
# Rename location columns to place so they can be joined together
# Name count column after variable they represent
# Thus, source goes from being a place to a count etc.
per_source <- letters %>%
  group_by(source) %>%
  rename(place = source) %>% 
  summarise(source = n(),
            correspondents = n_distinct(writer)) %>%
  remove_missing()

per_destination <- letters %>%
  group_by(destination) %>%
  rename(place = destination) %>% 
  summarise(destination = n()) %>%
  remove_missing()

# Join the three above data frames together and join with location data
cities <- full_join(per_source, per_destination, by = "place") %>% # keep all items in both tables
  left_join(locations, by = "place") %>% 
  replace_na(list(source = 0, destination = 0, correspondents = 0))
    # replace NAs with 0s in count columns

# Filter cities data frame to have separate data frames with sources and destinations
# Both data frames have all information for the locations
sources <- filter(cities, source > 0)
destinations <- filter(cities, destination > 0)

# Color palette
pal <- colorNumeric(palette = "viridis", domain = gcircles_routes$count, reverse = TRUE)

# Labels: make separate labels for sources and destinations to avoid mismatches
label1 <- sprintf(
  "%s to %s<br/>Letters: %g",
  gcircles_routes$source, gcircles_routes$destination, gcircles_routes$count
) %>% lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  destinations$place, destinations$source, destinations$destination, destinations$correspondents
) %>% lapply(htmltools::HTML)

label3 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  sources$place, sources$source, sources$destination, sources$correspondents
) %>% lapply(htmltools::HTML)

# Plot
leaflet(gcircles_routes) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addCircleMarkers(data = destinations, lng = ~lon, lat = ~lat,
      color = "#addd8e", stroke = FALSE, fillOpacity = 1, radius = 8, group = "Destinations",
      label = label2,
      labelOptions = labelOptions(textsize = "11px")) %>%
  addCircleMarkers(data = sources, lng = ~lon, lat = ~lat,
      color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 5, group = "Sources",
      label = label3,
      labelOptions = labelOptions(textsize = "11px")) %>%
  addPolylines(opacity = 0.8, weight = 3, color = ~pal(count), group = "Routes",
      label = label1,
      labelOptions = labelOptions(textsize = "11px"),
      highlight = highlightOptions(weight = 5, color = "#ff9600", opacity = 1)) %>%
  addLegend(position = "topright",
      colors = c("#ffd24d", "#addd8e"),
      labels = c("Sent Location", "Received Location"),
      opacity = 1) %>%
  addLegend(pal = pal, values = ~count, opacity = 1,
      title = "Letters<br/>Received") %>% 
  addLayersControl(position = "bottomright",
      overlayGroups = c("Destinations", "Sources", "Routes"),
      options = layersControlOptions(collapsed = FALSE))
