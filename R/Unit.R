# alternative to ontology annotation when using a quantity not a controlled vocab
# value handling? - where to put the validation?
#
# have a warnings / messages field that can indicate if you are not using an ontology and
# show this in the UI?

# hard code units of measurement ontology?
# unit annotation
# unit source

#' R6 object for specifying a unit of measurement
#'
Unit <- R6::R6Class(
	"Unit",
	public = list(
		unit = NULL,
		initialize = function(unit = NULL) {
			if (is.null(unit)) { self$unit <- unit } else {
				self$unit <- OntologyAnnotation$new(unit, OM)
			}
		}
	)
)


UnitAnnotation <- R6::R6Class(
	"UnitAnnotation",
	public = list(
		value = NULL,
		unit = NULL,
		check_function = NULL,
		initialize = function(
			value = NULL,
			unit = NULL,
			check_function = NULL
		) {
			if(is.null(value)) { self$value <- value } else { self$set_value(value) }
			if(is.null(unit)) { self$unit <- unit } else { self$set_unit(unit)}
			if(is.null(check_function)) {
				self$check_function <- function(x) {TRUE}
			} else {
				self$check_function <- check_function
			}
		},
		check_unit = function(unit) {
			check <- checkmate::check_r6(unit, "Unit")
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		set_unit = function(unit) {
			if(self$check_unit(unit)) { self$unit <- unit }
		},
		check_value = function(value) {
			check <- self$check_function(value)
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		set_value = function(value) {
			if(self$check_value(value)) { self$value <- value }
		}
	)
)


metre <- Unit$new("metre")
UnitAnnotation$new(1,metre)
