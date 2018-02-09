## ggmap great circle routes ##

library(tidyverse)
library(ggmap)

# Load data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
routes <- read_rds("data/gcircles_routes_tb.rds")

# Prepare the data

geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables

per_source <- letters %>%
  group_by(source) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  arrange(count)

per_destination <- letters %>%
  group_by(destination) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  arrange(count)

geo_per_destination <- inner_join(geo_data, per_destination, by = c("place" = "destination"))
geo_per_source <- inner_join(geo_data, per_source, by = c("place" = "source"))

# Get background map
bw_map <- get_googlemap(center = c(3.5, 46.5), zoom = 5,
  color = "bw",
  style = "feature:road|visibility:off&style=element:labels|visibility:off&
  style=feature:administrative|visibility:off")

dark_map <- get_googlemap(center = c(3.5, 46.5), zoom = 5,
  style = "feature:road|visibility:off&style=element:labels|visibility:off&
  style=feature:administrative|visibility:off&style=element:geometry|color:
  0x023e58&style=feature:landscape.natural|color:0x023e58&style=feature:water|color:0x0e1626")

### Map ###
ggmap(bw_map, extent = "device", legend = "topright") +
  geom_point(aes(x = lon, y = lat), color = "#addd8e", alpha = .9, size = 4, data = geo_per_destination) +
  geom_point(aes(x = lon, y = lat), color = "#ffe79e", alpha = .9, size = 2, data = geo_per_source) +
  geom_line(data = routes, aes(x = long, y = lat, group = group, color = count), size = .7) +
  scale_colour_distiller(palette = "YlOrRd", direction = 1, name = "Letters\nReceived", guide = "colorbar")

### Map without need for extra data ###
ggmap(bw_map, extent = "device", legend = "topright") +
  geom_point(data = routes, aes(x = lon.y, y = lat.y), color = "#addd8e", alpha = .9, size = 4) +
  geom_point(data = routes, aes(x = lon.x, y = lat.x), color = "#ffe79e", alpha = .9, size = 2) +
  geom_line(data = routes, aes(x = long, y = lat, group = group, color = count), size = 0.7) +
  scale_colour_distiller(palette = "YlOrRd", direction = 1, name = "Letters\nReceived", guide = "colorbar") +
  labs(title = "Daniel van der Meulen Correspondence, 1578-1591")
