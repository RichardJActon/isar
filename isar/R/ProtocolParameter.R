#' A parameter used by a protocol.
#'
#' @field parameter_name A parameter name as an ontology term
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
ProtocolParameter <- R6::R6Class(
	"ProtocolParameter",
	public = list(
		parameter_name = NULL,
		comments = NULL,
		#' @details
		#' Create a new \code{[ProtocolParameter]} object
		#' @param parameter_name A parameter name as an ontology term
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			parameter_name = NULL,
			comments = NULL
		) {
			if(is.null(parameter_name)) {
				self$parameter_name <- parameter_name
			} else {
				self$set_parameter_name(parameter_name)
			}
			self$comments <- comments
		},
		#' @details
		#' Check that parameter_name is an \code{[OntologyAnnotation]} object
		#' @param parameter_name an \code{[OntologyAnnotation]} object
		check_parameter_name = function(parameter_name) {
			check <- checkmate::check_r6(parameter_name, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set parameter_name if input is valid
		#' @param parameter_name an \code{[OntologyAnnotation]} object
		set_parameter_name = function(parameter_name) {
			if(self$check_parameter_name(parameter_name)) {
				self$parameter_name <- parameter_name
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
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		#' @examples
		#' Person$new()
		to_list = function(ld = FALSE, recursive = FALSE) {
			protocol_parameter = list(
				"id" = private$id,
				"parameter_name" = switch(
					as.character(recursive),
					"TRUE" = self$parameter_name$to_list(),
					"FALSE" = self$parameter_name$term
				),
				"comments" = self$comments
			)
			return(protocol_parameter)
		},

		#' @details
		#'
		#' Make \code{[Person]} from list
		#'
		#' @param lst an \code{[Person]} object serialized to a list
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		from_list = function(lst, recursive = FALSE) {
			private$id <- lst[["id"]]
			if(recursive) {
				self$parameter_name <- OntologyAnnotation$new()
				self$parameter_name$from_list(lst[["parameter_name"]])
			} else {
				if(checkmate::test_r6(
					lst[["parameter_name"]], "OntologyAnnotation"
				)) {
					stop("not a list contains raw OntologyAnnotation object")
				} else if(is.null(lst[["parameter_name"]])) {
					self$parameter_name <- NULL
				} else {
					self$parameter_name$term <- lst[["parameter_name"]]
				}
			}
			self$comments <- lst[["comments"]]
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
		}
	),
	private = list(
		id = generate_id()
	)
)

#' identical.ProtocolParameter
#'
#' Allows checking for the identity of \code{[ProtocolParameter]} objects
#'
#' @param x a \code{[ProtocolParameter]} object
#' @param y a \code{[ProtocolParameter]} object
#' @export
identical.ProtocolParameter <- s3_identical_maker(c(
	"parameter_name",
	"comments"
))
