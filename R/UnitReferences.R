#' R6 class for UnitReferences
#'
#' @field units A list of units used as a [UnitReferences] object
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate test_list test_r6
#' @importFrom purrr map_lgl map set_names map_chr
#'
#' @export
UnitReferences <- R6::R6Class(
	"UnitReferences",
	public = list(
		units = NULL,
		ontology_source_references = NULL,
		#' @param units A list of units used as a [Unit] object
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
		initialize = function(
			units = NULL,
			ontology_source_references = NULL
		) {
			self$units <- units
			self$ontology_source_references <- ontology_source_references
		},
		#' @details
		#' Check if this input is a list of [Unit] objects
		#' @param units  a list of [Unit] objects
		check_unit_references = function(units) {
			if (
				checkmate::test_list(units, names = "unique", min.len = 1) &&
				#checkmate::test_list(units, min.len = 1) &&
				all(purrr::map_lgl(units, ~checkmate::test_r6(.x, "Unit")))
			) { return(TRUE) } else{
				stop("Unit references must be a uniquely named list of Unit objects!")
			}
		},
		#' @details
		#' Set new units of the unit reference, overwrites current units
		#' @param units a list of [Unit] objects
		set_unit_references = function(units) {
			if(self$check_unit_references(units)) {
				self$units <- units
			}
		},
		#' @details
		#' Add new units to the unit reference
		#' @param units a list of [Unit] objects
		add_unit_references = function(units) {
			# browser()
			# remove any duplicates
			# ur_lgl <- names(units) %in% names(self$unit_references)
			# units <- units[!ur_lgl]

			comb <- c(self$units, units)
			if(self$check_unit_references(comb)) {
				self$units <- comb
			}
		},
		#' @details
		#' Get the @ids of the unit references
		#' @param source the ids of the source of the units for which you would like to get ids
		#' @return character vector of unit reference @ids
		get_unit_ids = function(source = "any") {
			if(source == "any") {
				names(self$units)
			} else if (source %in% self$get_unit_origins()) {
				self$units %>%
					`[`(
						self$get_unit_origins() %in% source
					) %>%
					names()
			}
		},
		#' @details
		#' Get the types of the unit references
		#' @param source the ids of the source of the units for which you would like to get types
		#' @return character vector of unit types
		get_unit_types = function(source = "any") {
			if(source == "any") {
				self$units %>%
					purrr::map_chr(~.x$unit$term)
			} else if (source %in% self$get_unit_origins()) {
				self$units %>%
					`[`(
						self$get_unit_origins() %in% source
					) %>%
					purrr::map_chr(~.x$unit$term)
			}
		},
		#' @details
		#' get the source of the unit e.g. from which study's list of unit
		#' categories did this reference originate
		#' @return character vector of unit sources
		get_unit_origins = function() {
			purrr::map_chr(self$units, ~.x$source)
		},
		#' @details
		#' Generate an R list representation of a [UnitReferences] object
		#' @param source ids of sources of unit references for which
		#' to generate a list represenation of unit references. 
		#' "any" lists categories from all sources. 
		#' default = "any"
		#' @return An R list representation of a [UnitReferences] object
		to_list = function(source = "any") {
			if(source == "any") {
				self$units %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else if (source %in% self$get_unit_origins()) {
				self$units %>%
					`[`(
						self$get_unit_origins() %in% source
					) %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else { list() }
		},
		#' @details
		#' Make a [UnitReferences] object from list
		#' @param lst a [UnitReferences] object serialized to a list
		#' @param source the source of the [Unit] object, if it was it listed
		#' in study unit categories list which one?
		#' @param add (logical) add new ob
		from_list = function(lst, source = NA, add = FALSE) {
			# browser()
			ur <- lst %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				purrr::map(~{
					u <- Unit$new(
						ontology_source_references =
							self$ontology_source_references,
						# unit_references = self$unit_references,
						source = source
					)
					u$from_list(.x)
					u
				})
			if(add){
				self$add_unit_references(ur)
			} else {
				self$set_unit_references(ur)
			}
		},
		#' @details
		#' Pretty prints [UnitReferences] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Unit References"))
			purrr::walk(self$units, ~.x$print())
		}
	)
)
