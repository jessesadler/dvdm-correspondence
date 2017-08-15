### Histogram and Bar charts by year/date ###

library(tidyverse)

letters <- read_csv("data/dvdm-correspondence-1591.csv")

# Histogram of letters with fill by destination

letters_clean <- filter(letters, destination != "NA")

ggplot(letters_clean) +
  geom_histogram(mapping = aes(x = date, fill = destination), binwidth = 182.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Daniel van der Meulen Correspondence, 1578–1591",
       x = "Date", y = "Letters", fill = "Received\nLocation")

ggplot(letters_clean) +
  geom_histogram(mapping = aes(x = date), binwidth = 182.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Daniel van der Meulen Correspondence, 1578–1591",
       x = "Date", y = "Letters", fill = "Received\nLocation")

ggplot(letters_clean) +
  geom_bar(mapping = aes(x = year, fill = destination)) +
  labs(title = "Daniel van der Meulen Correspondence, 1578–1591",
       x = "Year", y = "Letters", fill = "Received\nLocation")
