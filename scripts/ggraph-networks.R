## ggraph plots ##

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)

### Create igraph object ###

letters <- read_csv("data/dvdm-correspondence-1591.csv")

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  arrange(desc(count)) %>% 
  ungroup()

# Distinct sources and destinations into a single tibble with id column as first column
sources <- per_route %>%
  distinct(source) %>%
  rename(place = source)

destinations <- per_route %>%
  distinct(destination) %>%
  rename(place = destination)

nodes <- full_join(sources, destinations) %>% 
  rowid_to_column("id")

# Create links with ids for source and destination and bring id columns to beginning of df
links <- per_route %>% 
  left_join(nodes, by = c("source" = "place")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "place")) %>% 
  rename(to = id) %>% 
  select(from, to, everything())

# Create igraph object
routes <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

# Alternatively can create tidygraph object
routes_tidy <- tbl_graph(nodes = nodes, edges = links, directed = TRUE)

### ggraph plots ###

# Set graph theme
set_graph_style()

# Basic plots
ggraph(routes) + geom_edge_link() + geom_node_point()

ggraph(routes, layout = "fr") + 
  geom_edge_fan(arrow = arrow(length = unit(1, 'mm')), 
                end_cap = circle(1, 'mm')) + 
  geom_edge_loop() + 
  geom_node_point()

ggraph(routes, layout = "fr") + 
  geom_edge_fan(aes(color = destination)) + 
  geom_edge_loop() + 
  geom_node_point() + 
  geom_node_text(aes(label = place))


# Arc graph
ggraph(routes, layout = 'linear') + 
  geom_node_point() +
  geom_edge_arc(aes(edge_alpha = count))

ggraph(routes, layout = 'linear') + 
  geom_node_point() +
  geom_edge_arc(aes(edge_alpha = count), fold = TRUE)

ggraph(routes, layout = 'linear') + 
  geom_node_point() +
  geom_edge_arc(aes(edge_width = count))

ggraph(routes, layout = 'linear') + 
  geom_node_point() +
  geom_edge_arc(aes(colour = count, alpha = count)) + 
  scale_edge_colour_distiller(palette = "PRGn")

ggraph(routes, layout = 'linear') + 
  geom_edge_arc(aes(alpha = ..index..)) + 
  scale_edge_alpha('Route direction', guide = 'edge_direction')

# Circle graph
ggraph(routes, layout = 'linear', circular = TRUE) + 
  geom_edge_arc()

ggraph(routes, layout = 'linear', circular = TRUE) + 
  geom_node_point() + 
  geom_edge_arc(aes(colour = count, alpha = ..index..)) + 
  scale_edge_alpha('Route direction', guide = 'edge_direction') + 
  scale_edge_colour_distiller(palette = "PRGn")
