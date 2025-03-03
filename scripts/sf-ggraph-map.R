## sf map with ggraph ##

library(tidyverse)
library(ggraph)
library(tidygraph)
library(sf)
library(rnaturalearth)

### Create tbl_graph object of routes ###
letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")
nodes <- read_csv("data/locations-1591.csv") %>% 
  select(1:3, x = lon, y = lat) %>% 
  rowid_to_column("id")

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  arrange(desc(count)) %>% 
  ungroup()

# Create links with ids for source and destination and bring id columns to beginning of df
links <- per_route %>% 
  left_join(nodes, by = c("source" = "place")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "place")) %>% 
  rename(to = id) %>% 
  select(from, to, count) %>% 
  arrange(count)

# Alternatively can create tidygraph object
routes <- tbl_graph(nodes = nodes, edges = links, directed = TRUE)

# Create background map
coastline_sf <- ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(sovereignt, continent)

# Make map with ggraph

# Coastlines with color and width
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point() + 
  geom_edge_fan(aes(color = count, width = count), alpha = 0.8) + 
  geom_edge_loop(aes(color = count), alpha = 0.8) + 
  scale_edge_color_distiller(palette = "Reds", direction = 1) +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  theme_graph()

# Coastlines with color, width, and viridis
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point() + 
  geom_edge_fan(aes(color = count, width = count), alpha = 0.8) + 
  geom_edge_loop(aes(color = count), alpha = 0.8) + 
  scale_edge_color_viridis(direction = -1) +
  scale_edge_width_continuous(range = c(0.3, 2)) + 
  theme_graph()

# Countries with color
ggraph(routes, layout = "nicely") + 
  geom_sf(data = countries_sf, fill = "gray", color = "gray") + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point(alpha = 0.7) + 
  geom_edge_fan(aes(color = count), alpha = 0.8) + 
  scale_edge_color_viridis(direction = -1) +
  theme_graph()

# Alpha for direction, width for count
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point(color = "gray") + 
  geom_edge_fan(aes(edge_width = count, alpha = ..index..)) + 
  scale_edge_width_continuous(range = c(0.3, 2)) +
  scale_edge_alpha('Route direction', guide = 'edge_direction') +
  theme_graph()

# Alpha for direction, color for count
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point(color = "gray") + 
  geom_edge_fan(aes(color = count, alpha = ..index..)) + 
  scale_edge_color_viridis(direction = 1) +
  scale_edge_alpha('Route direction', guide = 'edge_direction') +
  theme_graph()

# Color and arrow
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_node_point() + 
  geom_edge_fan(aes(color = count), arrow = arrow(length = unit(2, 'mm')), 
                end_cap = circle(1, 'mm')) + 
  scale_edge_color_viridis(direction = -1) +
  theme_graph()

# Without background map
ggraph(routes, layout = "nicely") + 
  geom_edge_fan(aes(color = count)) + 
  scale_edge_color_viridis(direction = -1) +
  theme_graph()

# With labels
ggraph(routes, layout = "nicely") + 
  geom_sf(data = coastline_sf) + 
  coord_sf(xlim = c(-5, 15), ylim = c(40, 55),
           datum = NA) +
  geom_edge_fan(aes(color = count), alpha = 0.8) + 
  geom_edge_loop(aes(color = count), alpha = 0.8) + 
  geom_node_point() + 
  geom_node_text(aes(label = place), repel = TRUE) +
  theme_graph()
