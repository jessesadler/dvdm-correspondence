### Geocode correspondence data ###

# Load libraries
library(tidyverse)
library(ggmap)

# Load letters data
letters <- read_csv("data/dvdm-correspondence-original.csv")

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

# Take out country name from places that needed it for geocoding
locations$place <- str_replace(locations$place, ",.*", "")

# Save as csv
write_csv(locations, "data/locations-year.csv")
locations <- read_csv("data/locations.csv")

# Take out country name from places that needed it for geocoding in letters data
letters$source <- str_replace(letters$source, ",.*", "")
letters$destination <- str_replace(letters$destination, ",.*", "")

# Save cleaned letters data as csv
write_csv(letters, "data/dvdm-correspondence-year.csv")
