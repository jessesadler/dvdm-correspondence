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
