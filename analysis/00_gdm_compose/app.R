library(shiny)

ui <- fluidPage(
  titlePanel("Gestational Diabetes Mellitus"),
  tabsetPanel(
    tabPanel("Load Annotations",
             tags$iframe(src = "http://localhost:4001",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Import Data", 
             tags$iframe(src = "http://localhost:4002",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Process Data",
             tags$iframe(src = "http://localhost:4003",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Principal Component Analysis",
             tags$iframe(src = "http://localhost:4004",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Differential Expression Analysis",
             tags$iframe(src = "http://localhost:4005",
                         style = "width:70vw;height:100vh;")),
    tabPanel("WGCNA Preparation",
             tags$iframe(src = "http://localhost:4006",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Network Construction",
             tags$iframe(src = "http://localhost:4007",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Export to Gephi",
             tags$iframe(src = "http://localhost:4008",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Gene Ontology Enrichment Analysis", 
             tags$iframe(src = "http://localhost:4009",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Module-Trait Correlations",
             tags$iframe(src = "http://localhost:4010",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Hub Connectivity",
             tags$iframe(src = "http://localhost:4011",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Hub Genes",
             tags$iframe(src = "http://localhost:4012",
                         style = "width:70vw;height:100vh;")),
    tabPanel("Gene Set Enrichment Analysis",
             tags$iframe(src = "http://localhost:4013",
                         style = "width:70vw;height:100vh;"))
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)