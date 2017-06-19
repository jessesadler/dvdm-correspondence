
library(shiny)
library(tidyverse)
library(sp)
library(geosphere)
library(data.table)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Load letters and geographic data
letters_geo <- read_csv("data/letters-geo.csv")

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({

    destination <- letters_geo %>% 
      filter(year >= input$range[1] & year <= input$range[2]) %>%
      group_by(destination, lon.y, lat.y) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count)

    source <- letters_geo %>% 
      filter(year >= input$range[1] & year <= input$range[2]) %>%
      group_by(source, lon.x, lat.x) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count)

       
    leaflet(data = letters_geo) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addCircleMarkers(data = destination, lng = ~lon.y, lat = ~lat.y,
              color = "#addd8e", stroke = FALSE, fillOpacity = .7, radius = ~count/5, popup = ~paste(destination, count)) %>% 
      addCircleMarkers(data = source, lng = ~lon.x, lat = ~lat.x,
              color = "#ffe79e", stroke = FALSE, fillOpacity = 1, radius = ~count/5, popup = ~paste(source, count))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  
})