### Minimal Shiny Leaflet Map of Daniel van der Meulen Correspondence ###

library(shiny)
library(tidyverse)
library(leaflet)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")

shinyServer(function(input, output, session) {

    ## Cities: To be used for labels on map
  cities <- reactive({
    
    filtered_letters <- letters[letters$year < input$dates, ]
    
    per_source <- filtered_letters %>%
      group_by(source) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count)
    
    cities <- inner_join(locations, per_source, by = c("place" = "source"))
    
    return(cities)
  })  
 
  # Output base map with legends
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(3.5, 46.5, zoom = 5)
  })
  
  # CircleMarkers
  observe({

    leafletProxy("map", data = cities()) %>%
      clearMarkers() %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 10)
  })
  })