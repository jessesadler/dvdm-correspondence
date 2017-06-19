library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 50,
                sliderInput("range", "Years", min(1578), max(1591),
                            value = range(1578, 1591), step = 1, sep = ""))
)
)