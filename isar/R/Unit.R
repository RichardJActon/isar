# alternative to ontology annotation when using a quantity not a controlled vocab
# value handling? - where to put the validation?
#
# have a warnings / messages field that can indicate if you are not using an ontology and
# show this in the UI?
#
# # Issues in proper unit composition handling
#
# measurement ontology is somewhat inconsistent
# unit
# prefix (conversions?)
# compound units "meters per second"
# multiplication, division, exponentiation (sign), order or operations/parens/groups
#
# unit of X (X as an amount, physical quantity etc.)
#
# (meter)(division)(second)
#
#
# isomorphisms? m/s vs ms^-1?
#
# ###!! keep it simple and add advanced handling later? !!##
#
# hard code units of measurement ontology?
# unit annotation
# unit source

#' R6 object for specifying a unit of measurement
#' @field unit a unit of measurement
Unit <- R6::R6Class(
	"Unit",
	public = list(
		unit = NULL,
		`@id` = character(),
		ontology_source_references = NULL,
		source = NA,
		#' @details
		#' Create a new \code{[Unit]} object
		#' @param unit a unit of measurement
		initialize = function(
			unit = NULL,
			`@id` = character(),
			ontology_source_references = NULL,
			source = NA
		) {
			if (is.null(unit)) { self$unit <- unit } else {
				# need a list of ontology annotations to account for
				# composit units e.g. ms^-2 is meters and seconds
				# need a separate field for the unit and the ontology
				# annotation(s) used in the unit
				#self$unit <- OntologyAnnotation$new(unit, OM)
				self$set_unit_from_string(unit)
			}
			self$`@id` <- `@id`
			self$ontology_source_references <- ontology_source_references
			self$source <- source
		},
		set_unit_from_string = function(unit) {
			#browser()
			check <- checkmate::check_string(unit)
			error_with_check_message_on_failure(check)

			if (unit %in% OM$terms_list || unit %in% names(OM$terms_list)) {
				if(
					!"OM" %in%
					self$ontology_source_references$get_ontology_source_names()
				) {
					self$ontology_source_references$add_ontology_sources(
						list("OM" = OM)
					)
				}
			}

			if (unit %in% OM$terms_list) {
				self$unit <- OntologyAnnotation$new(
					term = unit,
					term_source = OM,
					ontology_source_references = self$ontology_source_references
				)
			} else if (unit %in% names(OM$terms_list)) {
				self$unit <- OntologyAnnotation$new(
					term_accession = unit,
					term_source = OM,
					ontology_source_references = self$ontology_source_references
				)
			} else {
				self$unit <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
				self$unit$set_valid_annotation(
					term = unit, term_accession = NULL,
					term_source_name = "UnknownSource"
				)
			}
		},
		get_unit_string = function() {
			self$unit$term
		},
		#' @details
		#' An R list representation of a \code{[Unit]} object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			lst <- list("@id" = self$`@id`)
			lst <- c(lst, self$unit$to_list())
			return(lst)
		},
		#' @details
		#' Make \code{[Unit]} object from list
		#' @param lst an Unit object serialized to a list
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			self$`@id` <- lst[["@id"]]
			self$set_unit_from_string(lst[["annotationValue"]])
			# self$unit <- OntologyAnnotation$new(
			# 	ontology_source_references = self$ontology_source_references
			# )
			#self$unit <- self$unit$from_list(lst)
		}
	)
)

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
