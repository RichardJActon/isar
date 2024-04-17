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
		comments = NULL, # comment
		`@id` = character(),
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
			term = character(), # str
			term_source = NULL, # OntologySource
			term_accession = character(), # str
			comments = NULL,
			`@id` = character()
		) {
			# transition to character() !!
			if (
				checkmate::test_string(term, max.chars = 0, min.chars = 0, null.ok = TRUE) &&
				!checkmate::test_string(term_accession, max.chars = 0, min.chars = 0, null.ok = TRUE)
			) {
				self$set_term_accession(term_accession)
			}

			if (is.null(term_source)) {
				self$term_source <- term_source # OntologySource
			} else {
				self$set_term_source(term_source)
			}

			if (
				!checkmate::test_string(term, max.chars = 0, min.chars = 0, null.ok = TRUE) &&
				checkmate::test_string(term_accession, max.chars = 0, min.chars = 0, null.ok = TRUE)
			) {
				self$set_term(term)
			}

			if (
				!checkmate::test_string(term, max.chars = 0, min.chars = 0, null.ok = TRUE) &&
				!checkmate::test_string(term_accession, max.chars = 0, min.chars = 0, null.ok = TRUE)
			) {
				if (self$term_source$terms_list[[term]] != term_accession) {
					stop("Supplied term & term accession do not match!")
				} else {
					self$set_term(term)
				}
			}

			if (
				checkmate::test_string(term, max.chars = 0, min.chars = 0, null.ok = TRUE) &&
				checkmate::test_string(term_accession, max.chars = 0, min.chars = 0, null.ok = TRUE)
			) {
				term <- character()
				term_accession <- character()
			}
			#-self$set_term(term)

			self$`@id` <- `@id`
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
		#' Add comment if it is in a valid format
		#' @param comment a list of comments
		add_comment = function(comment) {
			if(self$check_comments(comment)) {
				self$comments <- c(comments, comment)
			}
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @param recursive call to_list methods of any objects within this object (default FALSE)
		to_list = function(ld = FALSE, recursive = TRUE) {
			ontology_annotation = list(
				"id" = private$id,
				"annotation_value" = self$term,
				"term_source" = switch(
					as.character(recursive),
					"TRUE" = self$term_source$to_list(),
					"FALSE" = switch(
						as.character(is.null(self$term_source)),
						"TRUE" = NULL, "FALSE" = self$term_source$name
					)
				),
				"term_accession" = self$term_accession,
				"comments" = self$comments ## !!
			)
			return(ontology_annotation)
		},

		#' @details
		#' Make \code{[OntologyAnnotation]} from list
		#' @param lst an ontology source object serialized to a list
		#' @param recursive call to_list methods of any objects within this object (default FALSE)
		from_list = function(lst, recursive = TRUE, json = FALSE) {
			if(json) {
				# recursive?
				self$term_source$name <- lst[["termSource"]]
				self$term_accession <- lst[["termAccession"]]
				self$term <- lst[["annotationValue"]]
				if(!is.null(lst[["comments"]])) {
					self$comments <- lst[["comments"]]
				}
				self$`@id` <- lst[["@id"]]
			} else {
				private$id <- lst[["id"]]
				self$term <- lst[["annotation_value"]]
				if(recursive) {
					self$term_source <- OntologySource$new()
					if(!is.null(lst[["term_source"]])) {
						self$term_source$from_list(lst[["term_source"]])
					}
				} else {
					if(checkmate::test_r6(
						lst[["term_source"]], "OntologySource"
					)) {
						stop("not a list contains raw OntologySource object")
					} else if(is.null(lst[["term_source"]])) {
						self$term_source <- NULL
					} else {
						self$term_source$name <- lst[["term_source"]]
					}
				}
				self$term_accession <- lst[["term_accession"]]
				self$comments <- lst[["comments"]]
			}
		},

		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		},

		#' @details
		#' set the uuid of this object
		#' @param id a uuid
		#' @param suffix a human readable suffix
		set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
			private$id <- generate_id(id, suffix)
		},

		print = function() {
			cli::cli_h1(cli::col_blue("Ontology Annotation"))
			green_bold_name_plain_content("Term", self$term)
			green_bold_name_plain_content("Term Accession", self$term_accession)
			green_bold_name_plain_content("Term Source", self$term_source$name)
			green_bold_name_plain_content("@id", self$`@id`)
			green_bold_name_plain_content("ID", private$id)
			pretty_print_comments(self$comments)
		}
	),
	private = list(
		id = generate_id()
	)
)

#' identical.OntologyAnnotation
#'
#' Allows checking for the identity of \code{[OntologyAnnotation]} objects
#'
#' @param x a \code{[OntologyAnnotation]} object
#' @param y a \code{[OntologyAnnotation]} object
#' @export
identical.OntologyAnnotation <- s3_identical_maker(c(
	"annotation_value",
	"term_source",
	"term_accession",
	"comments"
))
