#' R6 class for an experimental factor
#'
#' A \code{[StudyFactor]} corresponds to an independent variable manipulated by the
#' experimentalist with the intention to affect biological systems in a way
#' that can be measured by an assay.
#'
#'
#' @field name The name of the factor
#' @field factor_type An \code{[OntologyAnnotation]} reference of the study factor type
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom checkmate qtest check_string
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#'
#' @export
StudyFactor <- R6::R6Class(
	"StudyFactor",
	public = list(
		name = character(),
		factor_type = NULL,
		comments = NULL,

		#' @details
		#'
		#' create a new study factor
		#'
		#' @param name The name of the factor
		#' @param factor_type An \code{[OntologyAnnotation]} reference of the study factor_type
		#' @param comments Comments associated with instances of this class.
		#'
		initialize = function(
			name = character(),
			factor_type = NULL,
			comments = NULL
		) {
			if (checkmate::qtest(name, "S[0]")) { self$name <- name } else {
				self$set_name(name)
			}
			if (is.null(factor_type)) {
				self$factor_type <- factor_type
			} else if (checkmate::check_r6(factor_type ,"OntologyAnnotation")) {
				self$factor_type <- factor_type
			} else {
				stop("factor_type is not and ontology_annotation object or NULL!")
			}
			self$comments <- comments
		},
		#' @details
		#' Check if the name of the material is a string
		#' @param name The name of the material
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the name of the material if valid
		#' @param name The name of the material
		set_name = function(name) {
			if (self$check_name(name)) { self$name <- name }
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
		#' @examples
		#' StudyFactor$new()
		to_list = function(ld = FALSE) {
			study_factor = list(
				"name" = self$name,
				"factor_type" = self$factor_type$to_list(),
				"id" = private$id,
				"comments" = self$comments
			)
			return(study_factor)
		},

		#' @details
		#'
		#' Make \code{[OntologyAnnotation]} from list
		#'
		#' @param lst a list serialization of a study factor object
		from_list = function(lst) {
			self$name = lst[["name"]]
			private$id <- lst[["id"]]
			self$factor_type <- OntologyAnnotation$new()
			self$factor_type$from_list(lst[["factor_type"]])
			self$comments = lst[["comments"]]
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
#' identical.StudyFactor
#'
#' Allows checking for the identity of \code{[StudyFactor]} objects
#'
#' @param x a \code{[StudyFactor]} object
#' @param y a \code{[StudyFactor]} object
#' @export
identical.StudyFactor <- s3_identical_maker(c(
	"name",
	"factor_type",
	"comments"
))
