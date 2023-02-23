
#' R6 class for sample
#'
#' Represents a sample material in an experimental graph.
#'
#' @section Public fields:
#' @field id ...
#' @field name A name/reference for the sample material.
#' @field factor_values A list of \code{[factor_values]} used to qualify the material in terms of study factors/design.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field derives_from A link to the source material that the sample is derived from.
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom checkmate check_string test_r6
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#'
#' @export
sample <- R6Class(
	"sample",
	public = list(
		id = '',
		name = '',
		factor_values = NULL,
		characteristics = NULL,
		derives_from = NULL,
		comments = NULL,

		#' @details
		#'
		#' Create a new instance of sample
		#'
		#' @param id ...
		#' @param name A name/reference for the sample material.
		#' @param factor_values A list of \code{[factor_value]} objects used to qualify the material in terms of study factors/design.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param derives_from A link to the source material that the sample is derived from.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			id = '',
			name = '',
			factor_values = NULL,
			characteristics = NULL,
			derives_from = NULL,
			comments = NULL
		) {
			self$id <- id
			self$set_name(name)
			if (is.null(factor_values)) {
				self$factor_values <- factor_values
			} else {
				self$set_factor_values(factor_values)
			}
			self$characteristics <- characteristics # list
			self$derives_from <- derives_from # list
			self$comments <- comments # list
		},

		# Checks

		#' @details
		#'
		#' validates the name field is a string
		#'
		#' @param name sample name to validate
		check_name = function(name) {
			# tryCatch(
			# 	{
					if (checkmate::check_string(name)) {
						return(TRUE)
					} else {
						#stop()
						stop("Name was not a string!")
					}
			# 	},
			# 	error = function(e) {
			# 		stop("Name was not a string!")
			# 		#message("Name was not a string!")
			# 	}
			# )
		},
		#' @details
		#'
		#' validates the factor_values field is a list of \code{[factor_value]} objects
		#'
		#' @param factor_values factor values to be used in a \code{[sample]} object
		#' A list of \code{[factor_value]} objects
		check_factor_values = function(factor_values) {
			if(!is.list(factor_values)) {
				stop(glue::glue(
					.sep = "\n",
					"factor_values is not a list!",
					"factor_values must be a list for factor_value objects"
				))
			}
			is_factor_value <- purrr::map_lgl(
				factor_values, ~checkmate::test_r6(.x, "factor_value")
			)
			if (all(is_factor_value)) {
				return(TRUE)
			} else {
				not_factor_values <- paste0(
					which(!is_factor_value), collapse = ", "
				)
				stop(glue::glue(
					.sep = "\n",
					"Not all factor_values are factor_value objects!",
					"Elements: {not_factor_values}",
					"are not factor values"
				))
			}

		},

		# Setters

		#' @details
		#'
		#' Sets the name
		#'
		#' @param name the name of the sample
		set_name = function(name) {
			if(self$check_name(name)) {
				self$name <- name
			}
		},
		#' @details
		#'
		#' Sets the factor values used in the sample
		#'
		#' @param factor_values factor values used in the sample
		#' A list of \code{[factor_value]} objects
		set_factor_values = function(factor_values) {
			if(self$check_factor_values(factor_values)) {
				self$factor_values <- factor_values
			}
		},

		#

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' sample$to_list()
		to_list = function(ld = FALSE) {
			sample = list(
				"id" = self$id,
				"name" = self$name,
				"factor_values" = purrr::map(self$factor_values, ~.x$to_list()),
				"characteristics" = self$characteristics,
				"derives_from" = self$derives_from,
				"comments" = self$comments
			)
			return(sample)
		},

		#' @details
		#'
		#' Make \code{[sample]} from list
		#'
		#' @param lst a list serialization of a \code{[sample]} factor object
		from_list = function(lst) {
			self$id <- lst[["id"]]
			self$name <- lst[["name"]]
			self$factor_values <- purrr::map(
				lst[["factor_values"]], ~{
					fv <- factor_value$new()
					fv$from_list(.x)
				}
			)
			self$characteristics <- lst[["characteristics"]]
			self$derives_from <- lst[["derives_from"]]
			self$comments <- lst[["comments"]]
		}
	)
)
