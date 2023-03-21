# commentable

#' R6 class for an experimental factor value
#'
#' @details
#' \code{[OntologySource]}
#'
#' @field term  the name of ontology term
#' @field term_source the ontology that is the source of the term represented by an \code{[OntologySource]} object.
#' @field term_accession the unique identifier of the ontology term
#' @field comments comments
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom shiny NS tagList verbatimTextOutput renderText moduleServer
#' @importFrom shinyWidgets pickerInput pickerOptions
#'
#' @export
OntologyAnnotation <- R6::R6Class(
	"OntologyAnnotation",
	public = list(
		term = NULL, # str
		term_source = NULL, # OntologySource
		term_accession = NULL, # str
		comments = list(), # comment

		#' @details
		#' create a new factor value
		#' @param term the name of ontology term
		#' @param term_source the ontology that is the source of the term represented by an \code{[OntologySource]} object.
		#' @param term_accession the unique identifier of the ontology term
		#' @param comments comments
		#' @param id a unique identifier ...
		#'
		#' @examples
		#' OA <- OntologyAnnotation$new()
		initialize = function(
			term = NULL, # str
			term_source = NULL, # OntologySource
			term_accession = NULL, # str
			comments = NULL # comment
		) {
			if (is.null(term_source)) {
				self$term_source <- term_source # OntologySource
			} else {
				self$set_term_source(term_source)
			}

			if (is.null(term) && !is.null(term_accession)) {
				self$set_term_accession(term_accession)
			}

			if (!is.null(term) && is.null(term_accession)) {
				self$set_term(term)
			}

			if (!is.null(term) && !is.null(term_accession)) {
				if (self$term_source$terms_list[[term]] != term_accession) {
					stop("Supplied term & term accession do not match!")
				} else {
					self$set_term(term)
				}
			}

			# if(!is.null(term)) {
			# 	# handling on not explicitly enumerated lists?
			# 	# - check a remote resource with an API call?
			# 	if(term %in% names(self$term_source$terms_list)) {
			# 		self$term <- term # str
			# 		if (is.null(term_accession)) {
			# 			self$term_accession <- self$term_source$terms_list[[term]]
			# 		} else if (self$term_source$terms_list[[term]] != term_accession) {
			# 			stop("Supplied term & term accession do not match!")
			# 		}
			# 	} else {
			# 		stop("term is not in term source")
			# 	}
			# }
			# if(!is.null(term_accession)) {
			# 	if(term_accession %in% unlist(self$term_source$terms_list)) {
			# 		self$term_accession <- term_accession # str
			# 		term_vec <- unlist(self$term_source$terms_list)
			# 		if (is.null(term)) {
			# 			self$term <- names(term_vec[term_vec == term_accession])
			# 		} else if (names(term_vec[term_vec == term_accession]) != term) {
			# 			stop("Supplied term accession & term do not match!")
			# 		}
			# 	} else {
			# 		stop("term accession is not in the term source")
			# 	}
			# }
			self$set_comments(comments)
			# invisible(self)
		},

		#' @details
		#' Checks that the source of ontology terms is an \code{[OntologySource]} object
		#' @param term_source an \code{[OntologySource]} object
		check_term_source = function(term_source) {
			check <- checkmate::check_r6(term_source, "OntologySource")
			error_with_check_message_on_failure(check)
		},

		#' @details
		#' Checks that the supplied term is in the list of valid terms from the ontology source object
		#' @param term an ontology term
		check_term = function(term) {
			if(term %in% names(self$term_source$terms_list)) {
				return(TRUE)
			} else {
				stop("term is not in term source")
			}
		},

		#' @details
		#' Checks that the supplied term accession is in the the list of valid accession terms from the ontology source object
		#' @param term_accession an accession for an ontology term
		check_term_accession = function(term_accession) {
			if(term_accession %in% unlist(self$term_source$terms_list)) {
				return(TRUE)
			} else {
				stop("term accession is not in the term source")
			}
		},

		#' @details
		#'
		#' Sets the value of term_source if it passes the checks
		#'
		#' @param term_source an \code{[OntologySource]} object
		set_term_source = function(term_source) {
			if(self$check_term_source(term_source)) {
				self$term_source <- term_source
			}
		},

		#' @details
		#' Sets the term and the term accession corresponding to that term if the term passes validity checks
		#' @param term an ontology term
		set_term = function(term) {
			if(self$check_term(term)) {
				self$term <- term
				self$term_accession <- self$term_source$terms_list[[term]]
			}
		},

		#' @details
		#' Sets the term accession and the term corresponding to that accession if the accession passes validity checks
		#' @param term_accession an accession for an ontology term
		set_term_accession = function(term_accession) {
			if(self$check_term_accession(term_accession)) {
				self$term_accession <- term_accession
				term_vec <- unlist(self$term_source$terms_list)
				self$term <- names(term_vec[term_vec == term_accession])
			}
		},

		# getters

		#  ## Shiny
		#  # unique ID?
		#
		#  #' @details
		#  #'
		#  #' Shiny UI element for picking an ontology term
		#  #'
		#  #' @param id Shiny module namespace
		#  #' @return a shiny UI element
		#  get_OntologyAnnotation_ui = function(id = "OntologyAnnotation") {
		#  	ns <- shiny::NS(id)
		#  	shiny::tagList(
		#  		shinyWidgets::pickerInput(
		#  			ns("measurement_type"), "Measurement Type",
		#  			choices = names(self$term_source$terms_list),
		#  			selected = self$term,
		#  			multiple = FALSE,
		#  			options = shinyWidgets::pickerOptions(
		#  				actionsBox = TRUE, liveSearch = TRUE#, size = 5
		#  			)
		#  		),
		#  		shiny::verbatimTextOutput(ns("measurement_type_test"))
		#  	)
		#  },
		#
		#  #' @details
		#  #'
		#  #' Shiny server element for picking an ontology term
		#  #'
		#  #' @param id Shiny module namespace
		#  #' @return a shiny UI element
		#  get_OntologyAnnotation_server = function(id) {
		#  	shiny::moduleServer(id, function(input, output, session) {
		#  		output$measurement_type_test <- shiny::renderText(input$measurement_type)
		#  		#self$term <- input$measurement_type
		#  		measurement_type <- shiny::reactive(input$measurement_type)
		#  		self$set_term(measurement_type())
		#  	})
		#  },
		#' @details
		#' checks if comments are a named list of character vectors
		#' @param comments comments
		check_comments = function(comments) { check_comments(comments) },
		#' @details
		#' Sets comments if they are in a valid format
		#' @param comments a list of comments
		set_comments = function(comments) {
			if(self$check_comments(comments)) { self$comments <- comments }
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
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
		#' Make \code{[OntologyAnnotation]} from list
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst) {
			self$id = lst[["@id"]]
			self$term = lst[["annotation_value"]]
			self$term_source <- OntologySource$new()
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
