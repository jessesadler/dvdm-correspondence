### Make great circles for routes:
### Export data as Spatial Lines Data Frame (sf) and as dataframe and tibble for ggplot ###

# Load libraries
library(tidyverse)
library(sp)
library(geosphere)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables

# Data from letters
per_route <- letters %>%
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count) %>%
  ungroup()

# Join data to locations data
geo_per_route <- per_route %>%
  left_join(geo_data, by = c("source" = "place")) %>% 
  left_join(geo_data, by = c("destination" = "place"))
geo_per_route$ID <- as.character(c(1:nrow(geo_per_route))) # create id for each pair

# Extract source and destination lat and lon data to be placed into gcIntermediate command
source_loc <- select(geo_per_route, lon.x, lat.x)
dest_loc <- select(geo_per_route, lon.y, lat.y)

# Calculate great circle routes between sources and destinations and return a SpatialLines object
routes <- gcIntermediate(source_loc, dest_loc, 100, addStartEnd=TRUE, sp=TRUE)

# Convert a SpatialLines object into SpatialLinesDataFrame, so that tabular data can be added

# create empty dataframe
ids <- data.frame()

# fill dataframe with IDs for each line
for (i in (1:length(routes))) {         
  id <- data.frame(routes@lines[[i]]@ID)
  ids <- rbind(ids, id)  }

colnames(ids)[1] <- "ID" # rename ID column, [1] says the first column is that which is to be renamed

# Convert SpatialLines into SpatialLinesDataFrame using IDs as the dataframe
# Only variable in the SpatialLinesDataFRame after this is ID

routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = TRUE)

### Join data as SpatialLinesDataFrame and save ###

gcircles_routes_sp <- merge(routes, geo_per_route, by = "ID")

write_rds(gcircles_routes_sp, "data/gcircles_routes_sp.rds")

### Tidy routes (convert to data frame) and join attributes ###
library(broom)

routes_df <- tidy(routes, region = "ID") # convert SpatialLinesDataFrame to a dataframe so it can be used by ggplot
routes_df <- rename(routes_df, ID = id) # rename id column to capitalized
gcircles_routes_df <- left_join(routes_df, geo_per_route, by = "ID")

write_rds(gcircles_routes_df, "data/gcircles_routes_df.rds")

## As tibble ##
gcircles_routes_tb <- as_tibble(gcircles_routes_df)

write_rds(gcircles_routes_tb, "gcircles_routes_tb.rds")
