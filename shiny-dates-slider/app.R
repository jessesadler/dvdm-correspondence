library(shiny)
library(leaflet)
library(dplyr)

# See https://stackoverflow.com/questions/40392676/r-shiny-date-slider-animation-by-month-currently-by-day/40402610#40402610

df <- data.frame(id = 1:5, 
                 lat = c(45.53814, 45.51076, 45.4356, 45.54332, 45.52234), 
                 lon = c(-73.63672, -73.61029, -73.6010, -73.56000, -73.59022),
                 startDate = as.Date(c("2014-04-09", "2014-06-04", "2014-04-30", "2014-05-30", "2014-05-01")),
                 year = c(2014, 2014, 2014, 2014, 2014),
                 month = c(4, 6, 4, 5, 5),
                 week = c(15, 23, 18, 22, 18),
                 ym = as.Date(c("2014-04-01", "2014-06-01", "2014-04-01", "2014-05-01", "2014-05-01")),  # Year-Month
                 yw = as.Date(c("2014-04-06", "2014-06-01", "2014-04-27", "2014-05-25", "2014-04-27"))   # Year-Week
)

# List of months
choices_month <- seq.Date(from = as.Date("2014-01-01"), by = "month", length.out = 36)


sliderValues <- function (inputId,
                          label,
                          values,
                          from,
                          to = NULL,
                          grid = TRUE,
                          width = NULL,
                          postfix = NULL,
                          prefix = NULL,
                          dragRange = TRUE,
                          disable = FALSE,
                          animate = FALSE) {
  validate_fromto <-
    function(fromto = NULL,
             values = NULL,
             default = 0) {
      if (!is.null(fromto)) {
        if (is.character(values) & is.numeric(fromto)) {
          fromto <- fromto - 1
        } else {
          fromto <- which(values == fromto) - 1
        }
      } else {
        fromto <- default
      }
      return(fromto)
    }
  
  sliderProps <- shiny:::dropNulls(
    list(
      class = "js-range-slider",
      id = inputId,
      `data-type` = if (!is.null(to))
        "double"
      else
        "single",
      `data-from` = validate_fromto(fromto = from, values = values),
      `data-to` = validate_fromto(
        fromto = to,
        values = values,
        default = length(values)
      ),
      `data-grid` = grid,
      `data-prefix` = if (is.null(prefix)) {
        "null"
      } else {
        shQuote(prefix, "sh")
      },
      `data-postfix` = if (is.null(postfix)) {
        "null"
      } else {
        shQuote(postfix, "sh")
      },
      `data-drag-interval` = dragRange,
      `data-disable` = disable,
      `data-values` = if (is.numeric(values)) {
        paste(values, collapse = ", ")
      } else {
        paste(shQuote(values, type = "sh"), collapse = ", ")
      }
    )
  )
  sliderProps <- lapply(
    X = sliderProps,
    FUN = function(x) {
      if (identical(x, TRUE))
        "true"
      else if (identical(x, FALSE))
        "false"
      else
        x
    }
  )
  sliderTag <- tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width))
      paste0("width: ", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label))
      shiny:::controlLabel(inputId, label),
    do.call(
      tags$input,
      list(
        type = if (is.numeric(values) &
                   is.null(to)) {
          "number"
        } else {
          "text"
        },
        #class = "js-range-slider",
        id = inputId,
        name = inputId,
        value = ""
      )
    ),
    tags$style(
      whisker::whisker.render(
        template =
          "input[id='{{id}}'] {
        -moz-appearance:textfield;
}
input[id='{{id}}']::-webkit-outer-spin-button,
input[id='{{id}}']::-webkit-inner-spin-button {
-webkit-appearance: none;
margin: 0;
}", data = list(id = inputId))
    ),
    tags$script(
      HTML(
        whisker::whisker.render(
          template = '$("#{{id}}").ionRangeSlider({
          type: "{{data-type}}",
          from: {{data-from}},
          to: {{data-to}},
          grid: {{data-grid}},
          keyboard: true,
          keyboard_step: 1,
          postfix: {{data-postfix}},
          prefix: {{data-prefix}},
          drag_interval: {{data-drag-interval}},
          values: [{{data-values}}],
          disable: {{data-disable}}
          });',
          data = sliderProps
      )
      ))
      )
  if (identical(animate, TRUE)) 
    animate <- animationOptions()
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- icon("play", lib = "glyphicon")
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- icon("pause", lib = "glyphicon")
    sliderTag <- htmltools::tagAppendChild(
      sliderTag,
      tags$div(class = "slider-animate-container", 
               tags$a(href = "#", class = "slider-animate-button", 
                      `data-target-id` = inputId, `data-interval` = animate$interval, 
                      `data-loop` = animate$loop, span(class = "play", 
                                                       animate$playButton), 
                      span(class = "pause", 
                           animate$pauseButton)))
    )
  }
  dep <- htmltools::htmlDependency(
    "ionrangeslider",
    "2.1.12",
    c(href = "shared/ionrangeslider"),
    script = "js/ion.rangeSlider.min.js",
    stylesheet = c(
      "css/ion.rangeSlider.css",
      "css/ion.rangeSlider.skinShiny.css"
    )
  )
  htmltools::attachDependencies(sliderTag, dep)
  }


# ui
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "75%", height = "100%"),
  
  absolutePanel(
    top = 1,
    right = 10,
    
    div(
      style = "height: 180px;",
      # custom slider function
      sliderValues(
        inputId = "test", label = "Month", width = "100%",
        values = choices_month[1:8], 
        from = choices_month[4], to = choices_month[6],
        grid = FALSE, animate = animationOptions(interval = 1500)
      ), # end sliderInput
      verbatimTextOutput("res")
    ) # end div
  ) # end absolutePanel
) # end bootstrapPage

server <- shinyServer(function(input, output, session){
  
  output$map <- renderLeaflet({
    leaflet(data = df %>% filter(ym > input$test[1], ym < input$test[2])) %>% addTiles() %>% 
   # leaflet(data = df %>% filter(ym == input$test[1])) %>% addTiles() %>% 
      addMarkers(~lon, ~lat) %>% 
      setView(lng = -73.6, lat = 45.52, zoom = 12)
  }) # end map
  
  output$res <- renderPrint({
    print(input$test) # you have to split manually the result by ";"
    print(as.Date(unlist(strsplit(input$test, ";"))))
  }) # end res
}) # end server

# App
shinyApp(ui = ui, server = server)


