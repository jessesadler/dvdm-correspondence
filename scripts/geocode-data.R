### Geocode correspondence data ###

# Geocode locations in the letters tibble
# Add historical regions
# Take out country name from places that need it in letters data

# Load libraries
library(dplyr)
library(stringr)
library(ggmap)

# Load letters data
# Load as a data frame so it can be geocoded
letters <- read.csv("data/dvdm-correspondence-1591.csv", stringsAsFactors = FALSE)

# Put ", country name" for necessary places for geocoding
letters$destination <- str_replace(letters$destination, "Brunswick", "Brunswick, Germany")
letters$source <- str_replace(letters$source, "Gravesend", "Gravesend, UK")
letters$source <- str_replace(letters$source, "Newbury", "Newbury, UK")
letters$source <- str_replace(letters$source, "Naples", "Naples, Italy")
letters$source <- str_replace(letters$source, "Rombeek", "Rombeek, Netherlands")

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

places <- full_join(sources, destinations, by = "place")

#Geocode with mutate_geocode
locations_df <- places %>% mutate_geocode(place, output = "more")

# Turn dataframe into tibble, select useful columns, and rename administrative area
locations <- as_tibble(locations_df) %>% 
  select(-(type:administrative_area_level_2)) %>% 
  select(place:country) %>% 
  rename(admin_area = administrative_area_level_1)

# Take out country name from places that needed it for geocoding
locations$place <- str_replace(locations$place, ",.*", "")

# Add historic regions
historic_regions <- tibble(
  country = c("Belgium", "France", "Germany", "Italy", "Netherlands", "United Kingdom"),
  historic_region = c("Spanish Netherlands", "France", "Germany", "Italy", "Dutch Republic", "England"))

locations <- left_join(locations, historic_regions)

# Save as csv
write_csv(locations, "data/locations-year.csv")
locations <- read_csv("data/locations.csv")

# Take out country name from places that needed it for geocoding in letters data
letters$source <- str_replace(letters$source, ",.*", "")
letters$destination <- str_replace(letters$destination, ",.*", "")

# Save cleaned letters data as csv
write_csv(letters, "data/dvdm-correspondence-year.csv")
