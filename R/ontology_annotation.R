# commentable

#' R6 class for an experimental factor value
#'
#' @details
#'  [ontology_source].
#'
#' @section Public fields:
#' @field term  the name of ontology term
#' @field term_source the ontology that is the source of the term represented by an [ontology_source] object.
#' @field term_accession the unique identifier of the ontology term
#' @field comments comments
#' @field id unique identifier...
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom shiny NS tagList verbatimTextOutput renderText moduleServer
#' @importFrom shinyWidgets pickerInput pickerOptions
#'
#' @export
ontology_annotation <- R6::R6Class(
	"ontology_annotation",
	public = list(
		term = NULL, # str
		term_source = NULL, # ontology_source
		term_accession = NULL, # str
		comments = list(), # comment

		#' @details
		#' create a new factor value
		#' @param term the name of ontology term
		#' @param term_source the ontology that is the source of the term represented by an [ontology_source] object.
		#' @param term_accession the unique identifier of the ontology term
		#' @param comments comments
		#' @param id a unique identifier ...
		#'
		#' @examples
		#' ontology_annotation$new(
		#'     term = "agrees with",
		#'     term_source = CiTO,
		#'     term_accession = "agreesWith"
		#' )
		initialize = function(
			term = NULL, # str
			term_source = NULL, # ontology_source
			term_accession = NULL, # str
			comments = list() # comment
		) {
			self$term_source <- term_source # ontology_source
			if(!is.null(term)) {
				# handling on not explicitly enumerated lists?
				# - check a remote resource with an API call?
				if(term %in% names(self$term_source$terms_list)) {
					self$term <- term # str
					if (is.null(term_accession)) {
						self$term_accession <- self$term_source$terms_list[[term]]
					} else if (self$term_source$terms_list[[term]] != term_accession) {
						stop("Supplied term & term accession do not match!")
					}
				} else {
					stop("term is not in term source")
				}
			}
			if(!is.null(term_accession)) {
				if(term_accession %in% unlist(self$term_source$terms_list)) {
					self$term_accession <- term_accession # str
					term_vec <- unlist(self$term_source$terms_list)
					if (is.null(term)) {
						self$term <- names(term_vec[term_vec == term_accession])
					} else if (names(term_vec[term_vec == term_accession]) != term) {
						stop("Supplied term accession & term do not match!")
					}
				} else {
					stop("term accession is not in the term source")
				}
			}
			self$comments <- comments # comment

			# invisible(self)
		},

		check_term_source = function(term_source) {
			check <- checkmate::check_r6(term_source, "ontology_source")
			if(check) { return(TRUE) } else { stop(check) }
		},
		check_term = function() {

		},
		check_term_accession = function() {

		},

		set_term_source = function(term_source) {
			if(check_term_source(term_source)) {
				self$term_source <- term_source
			}
		},

		# getters

		## Shiny
		# unique ID?

		#' @details
		#'
		#' Shiny UI element for picking an ontology term
		#'
		#' @param id Shiny module namespace
		#' @return a shiny UI element
		get_ontology_annotation_ui = function(id = "ontology_annotation") {
			ns <- shiny::NS(id)
			shiny::tagList(
				shinyWidgets::pickerInput(
					ns("measurement_type"), "Measurement Type",
					choices = names(self$term_source$terms_list),
					selected = self$term,
					multiple = FALSE,
					options = shinyWidgets::pickerOptions(
						actionsBox = TRUE, liveSearch = TRUE#, size = 5
					)
				),
				shiny::verbatimTextOutput(ns("measurement_type_test"))
			)
		},

		#' @details
		#'
		#' Shiny server element for picking an ontology term
		#'
		#' @param id Shiny module namespace
		#' @return a shiny UI element
		get_ontology_annotation_server = function(id) {
			shiny::moduleServer(id, function(input, output, session) {
				output$measurement_type_test <- shiny::renderText(input$measurement_type)
			})
		},

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' ontology_annotation$to_list()
		to_list = function(ld = FALSE) {
			ontology_annotation = list(
				"@id" = self$id,
				"annotation_value" = self$term,
				"term_source" = self$term_source$to_list(),
				"term_accession" = self$term_accession,
				"comments" = self$comments ## !!
			)
			return(ontology_annotation)
		},

		#' @details
		#'
		#' Make [ontology_annotation] from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst) {
			self$id = lst[["@id"]]
			self$term = lst[["annotation_value"]]
			self$term_source <- ontology_source$new()
			self$term_source$from_list(lst[["term_source"]])
			self$term_accession = lst[["term_accession"]]
			self$comments = lst[["comments"]]
		},

		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		}
	),
	private = list(
		id = uuid::UUIDgenerate()
	)
)
