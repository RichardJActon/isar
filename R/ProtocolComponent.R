#' An R6 object for \code{[ProtocolComponent]}s
#' A component used in a protocol.
#' @field name A component name.
#' @field component_type The classifier as a term for the component.
#' @field comments Comments associated with instances of this class.
ProtocolComponent <- R6::R6Class(
	"ProtocolComponent",
	public = list(
		name = NULL,
		component_type = NULL,
		comments = NULL,
		#' @details
		#' Create a new ProtocolComponent
		#' @param name A component name.
		#' @param component_type The classifier as a term for the component.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = NULL,
			component_type = NULL,
			comments = NULL
		) {
			if (checkmate::qtest(name, "S[0]")) { self$name <- name } else {
				self$set_name(name)
			}
			if (is.null(component_type)) {
				self$component_type <- component_type
			} else {
				self$set_component_type(component_type)
			}
			self$set_comments(comments)
		},
		#' @details
		#' Check that name is a single string
		#' @param name the name of the protocol
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set name if it passes validation
		#' @param name the name of the protocol
		set_name = function(name) {
			if(self$check_name(name)) { self$name <- name }
		},

		#' @details
		#' Check that component_type is an \code{[OntologyAnnotation]} object
		#' @param component_type an \code{[OntologyAnnotation]} object
		check_component_type = function(component_type) {
			check <- checkmate::check_r6(component_type, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set component_type if input is valid
		#' @param component_type an \code{[OntologyAnnotation]} object
		set_component_type = function(component_type) {
			if(self$check_pcomponent_type(component_type)) {
				self$component_type <- component_type
			}
		},
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
		#' An R list representation of a \code{[ProtocolComponent]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE){
			protocol_component <- list(
				"name" = self$name,
				"component_type" = self$component_type$to_list(),
				"comments" = self$comments
			)
			return(protocol_component)
		},
		#' @details
		#' Make \code{[Characteristic]} object from list
		#' @param lst an Characteristic object serialized to a list
		from_list = function(lst) {
			self$name <- lst[["name"]]
			self$component_type <- OntologyAnnotation$new()
			self$component_type$from_list(lst[["component_type"]])
			self$comments <- lst[["comments"]]
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
