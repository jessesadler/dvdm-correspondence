### Shiny Leaflet Map of Daniel van der Meulen Correspondence ###

library(shiny)
library(tidyverse)
library(sp)
library(sf)
library(geosphere)
library(leaflet)
library(htmltools)
library(RColorBrewer)

# Load letters and geographic data
letters <- read_csv("data/dvdm-correspondence-1591.csv")
locations <- read_csv("data/locations-1591.csv")
geo_data <- select(locations, place:lat) # simplify locations data to only necessary variables
# empty sf object of lines to use for periods with no data
empty_sf <- read_rds("data/empty_sf.rds")

# Set baseline for palette to be used for legend
routes_all <- letters %>% 
  group_by(source, destination) %>%
  summarise(count = n()) %>%
  remove_missing() %>%
  arrange(count)

shinyServer(function(input, output, session) {

  # Routes
  gcircles_routes <- reactive({

    per_route <- letters %>%
      filter(year >= input$range[1] & year < input$range[2]) %>% 
      group_by(source, destination) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count) %>% 
      ungroup()
    
    # Skip rest of function and return empty sf lines object if filtered data is empty
    if(nrow(per_route) < 1) {
      empty_sf
    } else {
    
    # Join data to locations data
    geo_per_route <- per_route %>%
      left_join(geo_data, by = c("source" = "place")) %>% 
      left_join(geo_data, by = c("destination" = "place"))
    geo_per_route$ID <- as.character(c(1:nrow(geo_per_route))) # create id for each pair
    
    source_loc <- select(geo_per_route, lon.x, lat.x)
    dest_loc <- select(geo_per_route, lon.y, lat.y)
    
    # Calculate great circle routes between sources and destinations and return a SpatialLines object
    routes <- gcIntermediate(source_loc, dest_loc, 100, addStartEnd=TRUE, sp=TRUE)
    
    # Convert a SpatialLines object into SpatialLinesDataFrame, so that tabular data can be added
    
    ids <- data.frame()

    for (i in (1:length(routes))) {         
      id <- data.frame(routes@lines[[i]]@ID)
      ids <- rbind(ids, id)  }
    
    colnames(ids)[1] <- "ID"
    
    routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = TRUE)
    
    # Create and return a SpatialLinesDataFrame
    merge(routes, geo_per_route, by = "ID")
    }
  })
    
  ## Cities: To be used for labels on map
  cities <- reactive({
  
    per_source <- letters %>%
      filter(year >= input$range[1] & year < input$range[2]) %>% 
      group_by(source) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count)
    
    per_destination <- letters %>%
      filter(year >= input$range[1] & year < input$range[2]) %>% 
      group_by(destination) %>%
      summarise(count = n()) %>%
      remove_missing() %>%
      arrange(count)
    
    corrs_per <- letters %>%
      filter(year >= input$range[1] & year < input$range[2]) %>% 
      group_by(source) %>%
      summarise(correspondents = n_distinct(name)) %>% 
      rename(place = source) %>% 
      arrange(desc(correspondents))
    
    geo_per_destination <- inner_join(geo_data, per_destination, by = c("place" = "destination"))
    geo_per_source <- inner_join(geo_data, per_source, by = c("place" = "source"))
    cities_temp <- full_join(geo_per_source, geo_per_destination, by = "place")
    
    left_join(cities_temp, corrs_per, by = "place") %>% 
      replace_na(list(count.x =0, count.y = 0, correspondents = 0)) # replace NAs with 0s in count columns
  })  
 
  # Output base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
      setView(3.5, 46.5, zoom = 5)
  })

  # Legend
  observe({
    pal <- colorNumeric(palette = "YlOrRd", domain = routes_all$count)
    
    leafletProxy("map", data = routes_all) %>%
      clearControls() %>% 
      addLegend(position = "topright",
                colors = c("#ffd24d", "#addd8e"),
                labels = c("Sent Location", "Received Location"),
                opacity = 1) %>%
      addLegend(pal = pal, values = ~count, opacity = 1,
                title = "Letters<br/>Received")
  })
  
  # CircleMarkers
  observe({
    # Return NA for labels if cities function is empty otherwise create labels
    label2 <- if(nrow(cities()) < 1) {
      NA
    } else {
    
    sprintf(
      "<strong>%s</strong><br/>Letters from: %g<br/>Letters to: %g<br/>Correspondents: %g",
      cities()$place, cities()$count.x, cities()$count.y, cities()$correspondents
    ) %>% lapply(htmltools::HTML)
    }

    leafletProxy("map", data = cities()) %>%
      clearMarkers() %>% 
      addCircleMarkers(lng = ~lon.y, lat = ~lat.y,
                       color = "#addd8e", stroke = FALSE, fillOpacity = 1, radius = 8,
                       label = label2,
                       labelOptions = labelOptions(textsize = "11px")) %>% 
      addCircleMarkers(lng = ~lon.x, lat = ~lat.x,
                       color = "#ffd24d", stroke = FALSE, fillOpacity = 1, radius = 5,
                       label = label2,
                       labelOptions = labelOptions(textsize = "11px"))
  })
  
  # Routes
  observe({
    pal <- colorNumeric(palette = "YlOrRd", domain = routes_all$count)
    
    # Return NA for labels if routes function is empty otherwise create labels
    label1 <- if(nrow(gcircles_routes()) < 1) {
      NA
    } else {
    
    sprintf(
      "%s to %s<br/>Letters: %g",
      gcircles_routes()$source, gcircles_routes()$destination, gcircles_routes()$count
    ) %>% lapply(htmltools::HTML)
    }
    
    leafletProxy("map") %>% 
      clearShapes() %>% 
      addPolylines(data = gcircles_routes(), opacity = 0.9, weight = 3, color = ~pal(count),
                   label = label1,
                   labelOptions = labelOptions(textsize = "11px"),
                   highlight = highlightOptions(weight = 5, color = "red", opacity = 1))
  })
  })