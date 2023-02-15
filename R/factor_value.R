#' R6 class for an experimental factor value
#'
#' @details
#' A factor_value represents the value instance of a [study_factor].
#'
#' @section Public fields:
#' * `factor_name`: name of a quantity
#' * `value`: value of a quantity
#' * `unit`: units in which that quantity is measured
#' * `comments`: comments
#'
factor_value <- R6Class(
	"factor_value",
	public = list(
		factor_name = '',
		value = '',
		unit = '',
		comments = NULL,

		#' @details
		#' create a new factor value
		#' @param factor_name the name of the experimental factor
		#' @param value the value of a quantity
		#' @param unit the unit of measurement
		#' @param comments comments
		#'
		#' @examples
		#' factor_value$new(
		#'     factor_name = "height",
		#'     value = "180", unit = "cm"
		#' )
		#' factor_value$new(
		#'     factor_name = sf,
		#'     value = "agrees with",
		#'     unit = NULL
		#' )

		initialize = function(
			factor_name = NULL,
			value = '',
			unit = NULL,
			comments = NULL
		) {
			self$factor_name <- factor_name
			if(is.null(factor_name)) { self$value <- value }
			if(
				!is.null(factor_name) &&
				value %in% names(
					self$factor_name$factor_type$term_source$terms_list
				)
			) {
				self$value <- value
			} else {
				stop(glue::glue(
					.sep = "\n",
					"Value is not in a valid terms for this type of factor",
					"see: factor_name$factor_type$term_source$terms_list"
				))
			}
			self$unit <- unit
			self$comments <- comments
		}
	)
)
