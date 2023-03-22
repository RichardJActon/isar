#' R6 class for sample
#'
#' Represents a sample material in an experimental graph.
#'
#' @field name A name/reference for the sample material.
#' @field factor_values A list of \code{[FactorValue]}s used to qualify the material in terms of study factors/design.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field derives_from A link to the source material that the sample is derived from.
#' @field comments Comments associated with instances of this class.
#'
#' @importFrom checkmate check_string test_r6
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#' @importFrom uuid UUIDgenerate
#' @importFrom R6 R6Class
#'
#' @export
Sample <- R6::R6Class(
	"Sample",
	inherit = Commentable,
	public = list(
		name = character(),
		factor_values = NULL,
		characteristics = NULL,
		derives_from = NULL,
		comments = NULL,

		#' @details
		#'
		#' Create a new instance of sample
		#'
		#' @param name A name/reference for the sample material.
		#' @param factor_values A list of \code{[FactorValue]} objects used to qualify the material in terms of study factors/design.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param derives_from A link to the source material that the sample is derived from.
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			name = character(),
			factor_values = NULL,
			characteristics = NULL,
			derives_from = NULL,
			comments = NULL
		) {
			self$set_name(name)
			if (is.null(factor_values)) {
				self$factor_values <- factor_values
			} else {
				self$set_factor_values(factor_values)
			}
			self$characteristics <- characteristics # list
			self$derives_from <- derives_from # list
			if(is.null(comments)) {
				self$comments <- comments
			} else {
				super$set_comments(comments)
			}
		},

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
		#' validates the factor_values field is a list of \code{[FactorValue]} objects
		#'
		#' @param factor_values factor values to be used in a \code{[Sample]} object
		#' A list of \code{[FactorValue]} objects
		check_factor_values = function(factor_values) {
			if(!is.list(factor_values)) {
				stop(glue::glue(
					.sep = "\n",
					"factor_values is not a list!",
					"factor_values must be a list for factor_value objects"
				))
			}
			is_factor_value <- purrr::map_lgl(
				factor_values, ~checkmate::test_r6(.x, "FactorValue")
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

		#' @details
		#'
		#' Sets the factor values used in the sample
		#'
		#' @param factor_values factor values used in the sample
		#' A list of \code{[FactorValue]} objects
		set_factor_values = function(factor_values) {
			if(self$check_factor_values(factor_values)) {
				self$factor_values <- factor_values
			}
		},

		#' @details
		#' check characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
		check_characteristics = function(characteristics) {
			if(
				checkmate::test_list(characteristics, min.len = 1) &&
				all(purrr::map_lgl(
					characteristics, ~checkmate::test_r6(.x, "Characteristic")
				))
			) { return(TRUE) } else {
				stop("All characteristics must be Characteristic objects")
			}
		},
		#' @details
		#' set characteristics if characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
		set_characteristics = function(characteristics) {
			if (self$check_characteristics(characteristics)) {
				self$characteristics <- characteristics
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
		#' @examples
		#' Sample$new()
		to_list = function(ld = FALSE) {
			sample = list(
				"id" = private$id,
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
		#' @param lst a list serialization of a \code{[Sample]} factor object
		from_list = function(lst) {
			private$id <- lst[["id"]]
			self$name <- lst[["name"]]
			self$factor_values <- purrr::map(
				lst[["factor_values"]], ~{
					fv <- FactorValue$new()
					fv$from_list(.x)
					fv
				}
			)
			self$characteristics <- lst[["characteristics"]]
			self$derives_from <- lst[["derives_from"]]
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
