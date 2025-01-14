library(shiny)
library(here)

ui <- shinyUI(
  fluidPage(
    includeHTML(here("www/02_load_data.html"))
  )
)

server <- function(input, output) {}

shinyApp(ui, server)
