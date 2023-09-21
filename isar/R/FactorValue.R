# make value a private method, if it should only come from factor type?

#' R6 class for an experimental factor value
#'
#' @details
#' A factor_value represents the value instance of a [StudyFactor].
#'
#' @field factor_name name of a quantity
#' @field value value of a quantity
#' @field unit units in which that quantity is measured
#' @field comments comments
#'
#' @importFrom checkmate check_r6
#'
#' @export
FactorValue <- R6Class(
	"FactorValue",
	public = list(
		factor_name = NULL,
		value = NULL,
		unit = NULL,
		comments = NULL,
		`@id` =  character(),
		#' @details
		#' create a new factor value
		#' @param factor_name the name of the experimental factor
		#' @param value the value of a quantity
		#' @param unit the unit of measurement
		#' @param comments comments
		#'
		#' @examples
		#' \dontrun{
		#' FactorValue$new(
		#'     factor_name = sf,
		#'     value = "agrees with",
		#'     unit = NULL
		#' )
		#' }

		initialize = function(
			factor_name = NULL,
			value = NULL,
			unit = NULL,
			comments = NULL,
			`@id` = character()
		) {
			if (!is.null(factor_name)) {
				if (checkmate::check_r6(factor_name, "StudyFactor")) {
					self$factor_name <- factor_name
				}
				self$value <- self$factor_name$factor_type
			} else {
				self$factor_name <-factor_name
				self$value <- value
			}
			# if (is.null(value)) {
			# 	self$value <- self$factor_name$factor_type
			# } else if (
			# 	checkmate::check_r6(value, "ontology_annotation") &&
			# 	value$term_accession == factor_name$factor_type$term_accession
			# ) {
			#
			# }

			# self$factor_name <- factor_name
			# if(is.null(factor_name)) { self$value <- value }
			# if(
			# 	!is.null(factor_name) &&
			# 	value %in% names(
			# 		self$factor_name$factor_type$term_source$terms_list
			# 	)
			# ) {
			# 	self$value <- value
			# } else {
			# 	stop(glue::glue(
			# 		.sep = "\n",
			# 		"Value is not in a valid terms for this type of factor",
			# 		"see: factor_name$factor_type$term_source$terms_list"
			# 	))
			# }
			self$unit <- unit
			self$comments <- comments
			self$`@id` <- `@id`
		},
		#' @details
		#' check if unit is a \code{[Unit]} object
		#' @param unit a \code{[Unit]} object
		check_unit = function(unit) {
			check <- checkmate::check_r6(unit, "Unit")
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set unit if input is valid
		#' @param unit a \code{[Unit]} object
		set_unit = function(unit) {
			if(self$check_unit(unit)) { self$unit <- unit }
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
		to_list = function(ld = FALSE) {
			factor_value = list(
				"factor_name" = self$factor_name$to_list(),
				"value" = self$value$to_list(),
				"unit" = self$unit,
				"comments" = self$comments
			)
			return(factor_value)
		},

		#' @details
		#'
		#' Make [OntologyAnnotation] from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst, recursive = FALSE, json = FALSE) {
			if(json){
				#self$set_id()
				self$factor_name <- StudyFactor$new()
				# self$factor_name$from_list(
				# 	lst[["factor_name"]], recursive = recursive, json = json
				# )
				self$`@id` <- lst[["category"]][["@id"]]
				self$factor_name$name <- sub(
					"#factor/", "", self$`@id`, fixed = TRUE
				)
				self$value <- OntologyAnnotation$new()
				self$value$from_list(
					lst[["value"]], recursive = recursive, json = json
				)
				self$set_comments(lst[["comments"]])
			} else {
				self$factor_name <- StudyFactor$new()
				self$factor_name$from_list(lst[["factor_name"]])
				# self$value <- self$factor_name$factor_type
				# !! not using value direct from list but from factor ~
				self$value <- OntologyAnnotation$new()
				self$value$from_list(lst[["value"]])
				self$unit <- lst[["unit"]]
				self$comments <- lst[["comments"]]
			}
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Factor Value"))
			green_bold_name_plain_content("Name", self$factor_name$name)
			green_bold_name_plain_content("Value", self$value$term)
			green_bold_name_plain_content("@id", self$`@id`)
			#green_bold_name_plain_content("ID", private$id)
			pretty_print_comments(self$comments)
		}
	)
)

#' identical.FactorValue
#'
#' Allows checking for the identity of \code{[FactorValue]} objects
#'
#' @param x a \code{[FactorValue]} object
#' @param y a \code{[FactorValue]} object
#' @export
identical.FactorValue <- s3_identical_maker(c(
	"factor_name",
	"value",
	"unit",
	"comments"
), get_id = FALSE)
