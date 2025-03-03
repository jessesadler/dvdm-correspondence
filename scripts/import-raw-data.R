## Letters 1578 -- 1591 ##

library(readr)
library(dplyr)

### Create data for letters up to end of 1591

letters <- read_csv("data/raw-letters-2017-09-26.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"),
  r_date = col_date(format = "%Y%m%d")))

# Filter letters
letters1591 <- filter(letters, year < 1592) %>% 
  select(-(language:notes))

# write out data to csv
write_csv(letters1591, "data-raw/dvdm-correspondence-1591.csv")
