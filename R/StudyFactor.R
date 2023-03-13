#' R6 class for an experimental factor
#'
#' A \code{[StudyFactor]} corresponds to an independent variable manipulated by the
#' experimentalist with the intention to affect biological systems in a way
#' that can be measured by an assay.
#'
#'
#' @field name The name of the factor
#' @field factor_type An \code{[OntologySource]} reference of the study factor type
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom checkmate check_r6
#' @importFrom R6 R6Class
#'
#' @export
StudyFactor <- R6::R6Class(
	"StudyFactor",
	public = list(
		name = '',
		factor_type = NULL,
		comments = NULL,

		#' @details
		#'
		#' create a new study factor
		#'
		#' @param name The name of the factor
		#' @param factor_type An \code{[OntologySource]} reference of the study factor_type
		#' @param comments Comments associated with instances of this class.
		#'
		initialize = function(
			name = '',
			factor_type = NULL,
			comments = NULL
		) {
			self$name <- name
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
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' StudyFactor$new()
		to_list = function(ld = FALSE) {
			study_factor = list(
				"name" = self$name,
				"factor_type" = self$factor_type$to_list(),
				"@id" = self$id,
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
			self$factor_type <- OntologyAnnotation$new()
			self$factor_type$from_list(lst[["factor_type"]])
			self$id = lst[["@id"]]
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
