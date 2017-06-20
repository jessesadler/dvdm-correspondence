### Leaflet map with sp great circle routes ###

# Load libraries
library(tidyverse)
library(sp)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
gcircles_routes <- read_rds("data/gcircles_routes_sp.rds")

# Prepare the data

geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables

per_source <- letters %>%
  group_by(source) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count)

per_destination <- letters %>%
  group_by(destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count)

corrs_per <- letters %>%
  group_by(source) %>%
  summarise(correspondents = n_distinct(name)) %>% 
  rename(place = source) %>% 
  arrange(desc(correspondents))

geo_per_destination <- inner_join(geo_data, per_destination, by = c("place" = "destination"))
geo_per_source <- inner_join(geo_data, per_source, by = c("place" = "source"))
cities <- full_join(geo_per_source, geo_per_destination, by = "place") # keep all items in both tables
cities <- left_join(cities, corrs_per, by = "place") %>% 
  replace_na(list(count.x =0, count.y = 0, correspondents = 0)) # replace NAs with 0s in count columns

# Color palette
pal <- colorNumeric(palette = "YlOrRd", domain = gcircles_routes$count)

# Labels
label1 <- sprintf(
  "%s to %s<br/>Number of letters: %g",
  gcircles_routes$source, gcircles_routes$destination, gcircles_routes$count
) %>% lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
  cities$place, cities$count.x, cities$count.y, cities$correspondents
) %>% lapply(htmltools::HTML)

# Plot
leaflet(gcircles_routes) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addCircleMarkers(data = cities, lng = ~lon.y, lat = ~lat.y,
      color = "#addd8e", stroke = FALSE, fillOpacity = 1, radius = 10,
      label = label2) %>%
  addCircleMarkers(data = cities, lng = ~lon.x, lat = ~lat.x,
      color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 5,
      label = label2) %>%
  addPolylines(opacity = 0.8, weight = 3, color = ~pal(count),
      label = label1,
      highlight = highlightOptions(weight = 5, color = "red", opacity = 1)) %>%
  addLegend(position = "topright",
      colors = c("#ffd24d", "#addd8e"),
      labels = c("Sent Location", "Received Location"),
      opacity = 1) %>%
  addLegend(pal = pal, values = ~count, opacity = 1,
            title = "Letters<br/>Received")
