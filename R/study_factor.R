#' R6 class for an experimental factors factors
#'
#' @details
#' A study_factor corresponds to an independent variable manipulated by the
#' experimentalist with the intention to affect biological systems in a way
#' that can be measured by an assay.
#'
#' @section Public fields:
#' * `id`: name of a quantity
#' * `name`: value of a quantity
#' * `factor_type`: An ontology source reference of the study factor type
#' * `comments`: Comments associated with instances of this class.
#'
study_factor <- R6Class(
	"study_factor",
	public = list(
		id = '',
		name = '',
		factor_type = ontology_annotation$new(),
		comments = NULL,

		#' @details Public fields:
		#'
		#' @param id: unique identifier...
		#' @param name: name of factor
		#' @param factor_type: An ontology source reference of the study factor type
		#' @param comments: Comments associated with instances of this class.
		#'

		initialize = function(
			id = '',
			name = '',
			factor_type = ontology_annotation$new(),
			comments = NULL
		) {
			self$id <- id
			self$name <- name
			self$factor_type <- factor_type
			self$comments <- comments
		}
	)
)
