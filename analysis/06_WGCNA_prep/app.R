library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/06_WGCNA_prep.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)

