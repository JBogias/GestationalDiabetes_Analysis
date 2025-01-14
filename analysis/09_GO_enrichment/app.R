library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/09_GO_enrichment.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)