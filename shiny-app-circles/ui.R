library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Date Range",
                            min(as.Date("1578-01-01", "%Y-%m-%d")),
                            max(as.Date("1591-12-31", "%Y-%m-%d")),
                            value = range(as.Date("1578-01-01", "%Y-%m-%d"), as.Date("1584-12-31", "%Y-%m-%d")),
                            timeFormat = "%e %b %Y"))
)
)