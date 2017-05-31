### Geocode correspondence data ###

# Load libraries
library(tidyverse)
library(ggmap)

# Load letters data
letters <- read_csv("data/dvdm-correspondence-1591.csv")

# The rest of the code makes a tibble (locations) with the geocoding of all of
# the places in the letters tibble and saves it as a rds document.

# Distinct sources and destinations into a single tibble
sources <- letters %>%
  distinct(source) %>%
  na.omit(source) %>%
  rename(place = source)

destinations <- letters %>%
  distinct(destination) %>%
  na.omit(destination) %>%
  rename(place = destination)

places <- full_join(sources, destinations)

# Make tibble a dataframe because of bug in mutate_geocode
places_df <- as.data.frame(places)

#Geocode with mutate_geocode
locations_df <- places_df %>% mutate_geocode(place, output = "more")

# Turn dataframe into tibble, select useful columns, and rename administrative area
locations <- as_tibble(locations_df) %>% 
  select(-(type:administrative_area_level_2)) %>% 
  select(place:country) %>% 
  rename(admin_area = administrative_area_level_1)

# Save tibble as rds document to maintain the type for each variable
write_rds(locations, "locations.rds")

# Load tibble from rds
locations <- read_rds("locations.rds")

# Could save to csv, which drops country and admin_area as factors
write_csv(locations, "locations.csv")
read_csv(locations, "locations.csv")