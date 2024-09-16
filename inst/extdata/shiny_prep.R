# library(shiny)
#
# ui <- fluidPage(
# 	#agrees_with$get_ontology_annotation_ui("oa")
# 	at$get_assay_ui("assay")
# )
#
# server <- function(input, output, session) {
# 	#agrees_with$get_ontology_annotation_server("oa")
# 	at$get_assay_server("assay")
# }
#
# shinyApp(ui, server)
#



tmpobjfile <- tempfile()

library(shiny)

ui <- fluidPage(
	#exOM$get_ontology_annotation_ui("example"),
	# shiny::uiOutput(ns("term_picker")),
	#ontology_annotation_ui("example"),
	shiny::uiOutput("example"),
	shiny::actionButton("save","save")
)

server <- function(input, output, session) {

	# exOM <- reactive(
	# 	ontology_annotation$new(
	# 		term = "metre",
	# 		term_source = OM
	# 	)
	# )
	#
	# output$term_picker <- shiny::renderUI(
	# 	exOM()$get_ontology_annotation_server("example")
	# )

	output$picker_ui <- shiny::renderUI(
		ontology_annotation_ui("", )
	)

	ontology_annotation_server("example")

	shiny::observeEvent(input$save, {
		qs::qsave(measurement_type(), tmpobjfile)
	})
}

shinyApp(ui, server)

ex <- qs::qread(tmpobjfile)
ex


##
# d OntologyAnnotation shiny
#
ontology_annotation_ui <- function(id = "ontology_annotation", OntologyAnnotation) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shinyWidgets::pickerInput(
			ns("measurement_type"), "Measurement Type",
			choices = names(OntologyAnnotation$term_source$terms_list),
			selected = OntologyAnnotation$term,
			multiple = FALSE,
			options = shinyWidgets::pickerOptions(
				actionsBox = TRUE, liveSearch = TRUE#, size = 5
			)
		),
		shiny::verbatimTextOutput(ns("measurement_type_test")),
	)
}

ontology_annotation_server <- function(id) {
	shiny::moduleServer(id, function(input, output, session) {
		output$measurement_type_test <- shiny::renderText(input$measurement_type)
		#self$term <- input$measurement_type
		measurement_type <- shiny::reactive(
			ontology_annotation$new(
				term_source = OM,
				term = input$measurement_type
			)
		)

		output$term_picker <- shiny::renderUI(
			ontology_annotation_ui("example", measurement_type())
		)
		# shiny::observeEvent(input$save, {
		# 	qs::qsave(measurement_type(), tmpobjfile)
		# })
	})
}
