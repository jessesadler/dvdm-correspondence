#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("DvdM Correspondence"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("dates", "Date",
                     min(1578),
                     max(1591),
                     value = 1584,
                     step = 1,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("barPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  per_destination <- reactive({
    letters %>%
    filter(year <= input$dates) %>%
    group_by(destination) %>%
    summarise(count = n()) %>%
    remove_missing() %>%
    arrange(count)
  })
    
   output$barPlot <- renderPlot({
       
     ggplot(per_destination()) +
       geom_histogram(mapping = aes(x = destination, stat = count)) +
       labs(title = "Daniel van der Meulen Correspondence, 1578–1591",
            x = "Year", y = "Letters", fill = "Received\nLocation")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

