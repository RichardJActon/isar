library(shiny)

ui <- fluidPage(
	#agrees_with$get_ontology_annotation_ui("oa")
	at$get_assay_ui("assay")
)

server <- function(input, output, session) {
	#agrees_with$get_ontology_annotation_server("oa")
	at$get_assay_server("assay")
}

shinyApp(ui, server)
