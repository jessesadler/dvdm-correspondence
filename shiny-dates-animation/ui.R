library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 60,
      sliderInput("dates", h3(style = "color:white", "Date"),
                  min(as.Date("1578-01-01", "%Y-%m-%d")),
                  max(as.Date("1591-12-31", "%Y-%m-%d")),
                  value = as.Date("1578-01-01", "%Y-%m-%d"),
                  step = 182.625, # half year
                  timeFormat = "%e %b %Y",
                  animate = TRUE
                  ))
)
)