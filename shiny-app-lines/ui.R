library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 0, left = 60,
                sliderInput("range", h3(style = "color:white", "Year Range"), min(1578), max(1592),
                            value = range(1578, 1579), step = 1, sep = "", animate=TRUE))
)
)