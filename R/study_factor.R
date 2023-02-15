#' R6 class for an experimental factor
#'
#' A \code{study_factor} corresponds to an independent variable manipulated by the
#' experimentalist with the intention to affect biological systems in a way
#' that can be measured by an assay.
#'
#' @details
#'
#'
#' @section Public fields:
#' * `name`: The name of the factor
#' * `factor_type`: An \code{[ontology_source]} reference of the study factor type
#' * `id`: unique identifier...
#' * `comments`: Comments associated with instances of this class.
#'
study_factor <- R6Class(
	"study_factor",
	public = list(
		name = '',
		factor_type = ontology_annotation$new(),
		id = '',
		comments = NULL,

		#' @details
		#'
		#' create a new study factor
		#'
		#' @param name The name of the factor
		#' @param factor_type An \code{[ontology_source]} reference of the study factor_type
		#' @param id unique identifier...
		#' @param comments Comments associated with instances of this class.
		#'
		initialize = function(
			name = '',
			factor_type = ontology_annotation$new(),
			id = '',
			comments = NULL
		) {
			self$name <- name
			self$factor_type <- factor_type
			self$id <- id
			self$comments <- comments
		}
	)
)
