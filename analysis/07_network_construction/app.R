library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/07_network_construction.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)

