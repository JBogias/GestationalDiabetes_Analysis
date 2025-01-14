library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/01_annotations.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
