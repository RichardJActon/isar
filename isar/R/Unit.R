# alternative to ontology annotation when using a quantity not a controlled vocab
# value handling? - where to put the validation?
#
# have a warnings / messages field that can indicate if you are not using an ontology and
# show this in the UI?

# hard code units of measurement ontology?
# unit annotation
# unit source

#' R6 object for specifying a unit of measurement
#' @field unit a unit of measurement
Unit <- R6::R6Class(
	"Unit",
	inherit = OntologySourceReferences,
	# inherit = OntologyAnnotation, # ?
	public = list(
		unit = NULL,
		`@id` = character(),
		#' @details
		#' Create a new \code{[Unit]} object
		#' @param unit a unit of measurement
		initialize = function(
			unit = NULL,
			`@id` = character()
		) {
			if (is.null(unit)) { self$unit <- unit } else {
				# need a list of ontology annotations to account for
				# composit units e.g. ms^-2 is meters and seconds
				# need a seperate field for the unit and the ontology
				# annotation(s) used in the unit
				self$unit <- OntologyAnnotation$new(unit, OM)
			}
			self$`@id` <- `@id`
		},
		#' @details
		#' An R list representation of a \code{[Unit]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			unit <- list(
				unit = self$unit$to_list(),
				"@id" = self$`@id`
			)
			return(unit)
		},
		#' @details
		#' Make \code{[Unit]} object from list
		#' @param lst an Unit object serialized to a list
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			self$`@id` <- lst[["@id"]]
			self$unit <- OntologyAnnotation$new()
			self$unit <- self$unit$from_list(lst)
		}
	)
)

#' identical.Unit
#'
#' Allows checking for the identity of \code{[Unit]} objects
#'
#' @param x a \code{[Unit]} object
#' @param y a \code{[Unit]} object
#' @export
identical.Unit <- s3_identical_maker(c("unit"))


#
# UnitAnnotation <- R6::R6Class(
# 	"UnitAnnotation",
# 	public = list(
# 		value = numeric(),
# 		unit = NULL,
# 		check_function = NULL,
# 		initialize = function(
# 			value = numeric(),
# 			unit = NULL,
# 			check_function = NULL
# 		) {
# 			if(is.null(value)) { self$value <- value } else { self$set_value(value) }
# 			if(is.null(unit)) { self$unit <- unit } else { self$set_unit(unit)}
# 			if(is.null(check_function)) {
# 				self$check_function <- function(x) {TRUE}
# 			} else {
# 				self$check_function <- check_function
# 			}
# 		},
# 		check_unit = function(unit) {
# 			check <- checkmate::check_r6(unit, "Unit")
# 			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
# 		},
# 		set_unit = function(unit) {
# 			if(self$check_unit(unit)) { self$unit <- unit }
# 		},
# 		check_value = function(value) {
# 			check <- checkmate::check_number(value)
# 			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
# 		},
# 		set_value = function(value) {
# 			if(self$check_value(value)) { self$value <- value }
# 		},
# 		to_list = function(ld = FALSE){
# 			UnitAnnotation <- list(
# 				value = self$value,
# 				unit = self$unit,
# 				check_function = self$check_function
# 			)
# 		},
# 		from_list = function(lst){
# 			self$value <- lst[["value"]]
# 			self$unit <- lst[["unit"]]
# 			self$check_function <- lst[["check_function"]]
# 		}
# 	)
# )


# metre <- Unit$new("metre")
# UnitAnnotation$new(1,metre)
