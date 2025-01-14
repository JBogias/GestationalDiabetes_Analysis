library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/04_PCA.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)