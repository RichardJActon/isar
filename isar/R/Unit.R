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
		unit_references = NULL,
		source = NA,
		#' @details
		#' Create a new \code{[Unit]} object
		#' @param unit a unit of measurement
		initialize = function(
			unit = NULL,
			`@id` = character(),
			ontology_source_references = NULL,
			unit_references = NULL,
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
			self$unit_references <- unit_references
			self$source <- source
		},
		set_valid_unit = function(term, term_accession, term_source) {
			# browser()
			if (!is.null(term)) {
				if (term %in% OM$terms_list || term %in% names(OM$terms_list)) {
					if(
						!"OM" %in%
						self$ontology_source_references$get_ontology_source_names()
					) {
						self$ontology_source_references$add_ontology_sources(
							list("OM" = OM)
						)
					}
				}
			}

			if (self$`@id` %in% self$unit_references$get_unit_ids()) {
				self$unit <- self$unit_references$unit_references[[self$`@id`]]
			} else {
				un <- Unit$new(
					ontology_source_references = self$ontology_source_references,
					unit_references = self$unit_references
				)
				un$set_valid_annotation(
					term = term, term_accession = term_accession,
					term_source_name = term_source
				)
				self$unit_references$add_unit_references(
					list("UnknownUnit" = un)
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
			self$set_valid_unit(
				lst[["annotationValue"]],
				lst[["termAccession"]],
				lst[["termSource"]]
			)
			# lst[["termAccession"]]
			# lst[["termSource"]]
			#self$set_unit_from_string(lst[["annotationValue"]])
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
