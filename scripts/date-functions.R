### Date functions ###

library(readr)
library(dplyr)
library(lubridate)

# Julian Calendar is the old calendar; Gregorian calendar is new calendar
# The old calendar was 10 days behind

# Load data
letters <- read_csv("data/dvdm-correspondence-1591.csv")

## Dates ##

# Math with dates: Difference between dates
ymd("1584-06-26") - ymd("1583-02-15")
as.integer(ymd("1584-06-26") - ymd("1583-02-15")) # extract number of days as integer

# Find days between sent and received
received <- letters %>% na.omit(r_date) # letters with sent and received dates
date_span <- mutate(received, span = r_date - date) # span as time function
date_span <- mutate(received, span = as.integer(r_date - date)) # span as time integer

## Transformation functions ##
old_to_new <- function(x) {ymd(x) + days(10)}
new_to_old <- function(x) {ymd(x) - days(10)}

gregorian <- old_to_new("1583-02-15")
julian <- new_to_old("1583-02-15")

## Mutate functions ##
old_calendar <- letters %>% mutate(Julian = date - days(10))

# Need tidyeval
old_calendar_mutate <- function(df) {mutate(df, Julian = date - days(10))}
new_calendar_mutate <- function(df) {mutate(df, Gregorian = date + days(10))}

old <- old_calendar_mutate(letters)
new <- new_calendar_mutate(letters)