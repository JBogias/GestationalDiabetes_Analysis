# App for ACR and App Service deployment
# I would just use the docker-compose for ACR but then I'd need to store 
# all seven of the images in the ACR, which would rake up costs
# App service is free though
library(shiny)
library(here)

addResourcePath(prefix = "html", directoryPath = "www/")

ui <- fluidPage(
  class = "fluid-page",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "html/styling.css")
  ),
  titlePanel(paste0("Code Companion for GDM WGCNA workflow",
                    " by Justin Bogias")),
  tabsetPanel(
    tabPanel("Prepare Annotations",
             class = "tab-panel",
             tags$iframe(src = "html/01_annotations.html",
                         style = "width:70vw;height:80vh;",
                         class = "markdown_frame")),
    tabPanel("Import Data",
             class = "tab-panel",
             tags$iframe(src = "html/02_load_data.html",
                         style = "width:70vw;height:80vh;",
                         class = "markdown_frame")),
    tabPanel("Data Processing",
             class = "tab-panel",
             tags$iframe(
               src = "html/03_process_data.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Principal Component Analysis",
             class = "tab-panel",
             tags$iframe(src = "html/04_PCA.html",
                         style = "width:70vw;height:80vh;",
                         class = "markdown_frame")),
    tabPanel("Differential Expression Analysis",
             class = "tab-panel",
             tags$iframe(src = "html/05_de.html",
                         style = "width:70vw;height:80vh;",
                         class = "markdown_frame")),
    tabPanel("WGCNA Preparation",
             class = "tab-panel",
             tags$iframe(
               src = "html/06_WGCNA_prep.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Network Construction",
             class = "tab-panel",
             tags$iframe(
               src = "html/07_network_construction.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Export to Gephi",
             class = "tab-panel",
             tags$iframe(
               src = "html/08_export_gephi.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Gene Ontology Enrichment Analysis",
             class = "tab-panel",
             tags$iframe(
               src = "html/09_GO_enrichment.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Module-Trait Correlations",
             class = "tab-panel",
             tags$iframe(
               src = "html/10_module_trait_correlations.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Hub Connectivity",
             class = "tab-panel",
             tags$iframe(
               src = "html/11_hub_connectivity.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Hub Genes",
             class = "tab-panel",
             tags$iframe(
               src = "html/12_hub_gene_plotting.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
    tabPanel("Gene Set Enrichment Analysis",
             class = "tab-panel",
             tags$iframe(
               src = "html/13_gene_set_enrichment.html",
               style = "width:70vw;height:80vh;",
               class = "markdown_frame")),
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)