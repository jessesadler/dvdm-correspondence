### Make great circles for routes as sf object ###

# Load libraries
library(tidyverse)
library(sp)
library(sf)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables

# Join letters data to locations data

geo_letters <- letters %>%
  inner_join(geo_data, by = c("source" = "place")) %>% 
  inner_join(geo_data, by = c("destination" = "place"))

geo_letters$ID <-as.character(c(1:nrow(geo_letters))) # create id for each pair

# Extract source and destination latitude and longitude data to be placed into gcIntermediate command
source_loc <- select(geo_letters, lon.x, lat.x)
dest_loc <- select(geo_letters, lon.y, lat.y)

# Calculate great circle routes between sources and destinations and return a SpatialLines object
routes <- gcIntermediate(source_loc, dest_loc, 100, addStartEnd=TRUE, sp=TRUE)

# Convert a SpatialLines object into SpatialLinesDataFrame, so that tabular data can be added

# create empty data frame
ids <- data.frame()

# fill data frame with IDs for each line
for (i in (1:length(routes))) {         
  id <- data.frame(routes@lines[[i]]@ID)
  ids <- rbind(ids, id)  }

colnames(ids)[1] <- "ID" # rename ID column, [1] says the first column is that which is to be renamed

# Convert SpatialLines into SpatialLinesDataFrame using IDs as the data frame
# Only variable in the SpatialLinesDataFRame after this is ID

routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = TRUE)

# Join geo_letters to routes and maintain as SpatialLinesDataFrame

gcircles_sp <- merge(routes, geo_letters, by = "ID")

# Transform sp object to sf object
letters_sf <- st_as_sf(gcircles_sp)

# Save data
write_rds(letters_sf, "letters_sf.rds")