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
	# inherit = Material,
	inherit = ReferencesInCommon,
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
			if (checkmate::qtest(name, "S[0]")) {
				self$name <- name
			} else {
				self$set_name(name)
			}

			if (is.null(factor_values)) {
				self$factor_values <- factor_values
			} else {
				self$set_factor_values(factor_values)
			}
			self$characteristics <- characteristics # list
			self$derives_from <- derives_from # list

			self$comments <- comments
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
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(!json) {
				private$id <- lst[["id"]]
			}
			self$name <- lst[["name"]]
			self$factor_values <- purrr::map(
				lst[["factorValues"]], ~{
					fv <- FactorValue$new()
					fv$from_list(.x, recursive = recursive, json = json)
					fv
				}
			)
			self$characteristics <- purrr::map(lst[["characteristics"]], ~{
				chr <- Characteristic$new()
				chr$from_list(.x, json = json, recursive = recursive)
				chr
			})

			self$derives_from <- purrr::map(lst[["derivesFrom"]], ~{
				if(.x %in% super$get_reference_names("Source")) {
					super$references[["Source"]][[.x]]
				} else {
					warning("Unknown material!")
					.x
				}
			})

			# self$derives_from <- lst[["derivesFrom"]]
			self$comments <- lst[["comments"]]
		},

		print = function() {
			cli::cli_h1(cli::col_blue("Sample"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("ID", private$id)
			cli::cli_h1(cli::col_green("Factor Categories"))
			cli::cli_ul(purrr::map_chr(self$factor_values, ~.x$`@id`))

			pretty_print_comments(self$comments)
		}
	),
	private = list(
		id = generate_id()
	)
)

