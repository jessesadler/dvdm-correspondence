## Create igraph and network objects ##

### Possible changes ###
# Could change per_route code to add years, which would add group to links data
# Could add country for each node from locations data, which would add group to nodes
# Could add total received and total sent from each location to nodes data

### igraph object ###

library(tidyverse)
library(igraph)

letters <- read_csv("data/dvdm-correspondence-1591.csv")

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(desc(count)) %>% 
  ungroup()

# Distinct sources and destinations into a single tibble with id column as first column
sources <- per_route %>%
  distinct(source) %>%
  rename(place = source)

destinations <- per_route %>%
  distinct(destination) %>%
  rename(place = destination)

nodes <- full_join(sources, destinations)
nodes <- add_column(nodes, id = 1:nrow(nodes))
nodes <- select(nodes, id, everything())

# Create links with ids for source and destination and bring id columns to beginning of df
links <- per_route %>% 
  left_join(nodes, by = c("source" = "place")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "place")) %>% 
  rename(to = id)

links <- select(links, from, to, everything())

# Create igraph object
routes_network <- graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

plot(routes_network)

# Circle plot
circle <- layout_in_circle(routes_network)
plot(routes_network, layout = circle)

### Network object ###

detach(package:igraph)
library(network)
rm(routes_network)

routes_network <- network(links, vertex.attr = nodes, matrix.type = "edgelist", 
                                  loops = TRUE, multiple = FALSE, ignore.eval = FALSE)

plot(routes_network)
