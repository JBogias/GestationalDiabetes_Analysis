library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/13_gene_set_enrichment.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)