### visNetwork visualizations ###

library(tidyverse)
library(visNetwork)

letters <- read_csv("data/dvdm-correspondence-1591.csv")

## Creating the links and nodes dataframes is the same as for igraph or network objects ##

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(desc(count)) %>% 
  ungroup()

# Nodes: Distinct sources and destinations into a single tibble with id column as first column
sources <- per_route %>%
  distinct(source) %>%
  rename(place = source)

destinations <- per_route %>%
  distinct(destination) %>%
  rename(place = destination)

nodes <- full_join(sources, destinations)
nodes <- add_column(nodes, id = 1:nrow(nodes))
nodes <- select(nodes, id, everything())

# Links: Create links with ids for source and destination and bring id columns to beginning of df
links <- per_route %>% 
  left_join(nodes, by = c("source" = "place")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "place")) %>% 
  rename(to = id)
links <- select(links, from, to, everything())

# Plot
visNetwork(nodes, links)

## Circle network
nodes$label <- nodes$place
links$width <- 1+links$count/5

visNetwork(nodes, links) %>% visIgraphLayout(layout = "layout_in_circle") %>% visEdges(arrows = "middle")

### Add historic regions to the network graph ###

locations <- read_csv("data/locations-1591.csv")

historic_regions <- tibble(
  country = c("Belgium", "France", "Germany", "Italy", "Netherlands", "United Kingdom"),
  historic_region = c("Spanish Netherlands", "France", "Germany", "Italy", "Dutch Republic", "England"))
locations <- left_join(locations, historic_regions)

nodes <- left_join(nodes, locations)

nodes$group <- nodes$historic_region

# Make color of the line as to rather than from or with viridis
