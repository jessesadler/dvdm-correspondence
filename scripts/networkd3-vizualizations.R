## Network visualizations ##

# This script is derived from http://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html

# Load packages and data
library(tidyverse)
library(networkD3)

letters <- read_csv("data-raw/dvdm-correspondence-1591.csv")

per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  arrange(desc(count)) %>% 
  ungroup()

# Distinct sources and destinations into a single tibble with id column
sources <- per_route %>%
  distinct(source) %>%
  rename(place = source)

destinations <- per_route %>%
  distinct(destination) %>%
  rename(place = destination)

nodes <- full_join(sources, destinations)
places <- nrow(nodes)
nodes <- add_column(nodes, id = 0:(places -1))

# Create links with ids for source and destination
links <- per_route %>% 
  left_join(nodes, by = c("source" = "place")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "place")) %>% 
  rename(to = id)

# Forced Network graph
forceNetwork(Links = links, Nodes = nodes, Source = "from", Target = "to", 
             Value = "count", NodeID = "place", Group = "id",
             fontSize = 14, opacity = 1, zoom = TRUE)

# Sankey Graph
sankeyNetwork(Links = links, Nodes = nodes, Source = "from", Target = "to", 
              Value = "count", NodeID = "place", fontSize = 16, unit = "letter(s)")
