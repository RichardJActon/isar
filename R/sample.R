
#' R6 class for
#'
#' Represents a sample material in an experimental graph.
#'
#' @section Public fields:
#' @field id ...
#' @field name A name/reference for the sample material.
#' @field factor_values A list of \cose{[factor_values]} used to qualify the material in terms of study factors/design.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field derives_from A link to the source material that the sample is derived from.
#' @field comments Comments associated with instances of this class.
#'
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
		#' @param factor_values A list of \code{[factor_values]} used to qualify the material in terms of study factors/design.
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
			self$factor_values <- factor_values
			self$characteristics <- characteristics
			self$derives_from <- derives_from
			self$comments <- comments
		},

		#' @details
		#'
		#' validates the name field is a string
		#'
		#' @param name sample name to validate
		check_name = function(name) {
			tryCatch(
				{
					if (is.character(name) && length(name) == 1L) {
						return(TRUE)
					} else {
						stop()
					}
				},
				error = function(e) {
					stop("Name was not a string!")
					#message("Name was not a string!")
				}
			)
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
		}
	)
)
