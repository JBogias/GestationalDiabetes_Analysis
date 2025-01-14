library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/11_hub_connectivity.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)