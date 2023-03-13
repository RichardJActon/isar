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
			comments = NULL
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
		from_list = function(lst) {
			self$factor_name <- StudyFactor$new()
			self$factor_name$from_list(lst[["factor_name"]])
			# self$value <- self$factor_name$factor_type
			# !! not using value direct from list but from factor ~
			self$value <- OntologyAnnotation$new()
			self$value$from_list(lst[["value"]])
			self$unit = lst[["unit"]]
			self$comments = lst[["comments"]]
		}
	)
)
