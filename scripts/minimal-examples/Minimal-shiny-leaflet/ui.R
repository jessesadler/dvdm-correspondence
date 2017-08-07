library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 0, left = 60,
      sliderInput("dates", "Years", min(1578), max(1592),
          value = 1585, step = 1, sep = ""))
)
)