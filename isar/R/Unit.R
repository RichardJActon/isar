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
#' @field @id identifier
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
#' @field source the source of the [OntologySource] object, was it listed in
# #' @field unit_references A list of units used as a [UnitReferences] object.
#'
#' @importFrom R6 R6Class
#'
#' @export
Unit <- R6::R6Class(
	"Unit",
	public = list(
		unit = NULL,
		`@id` = character(),
		ontology_source_references = NULL,
#		unit_references = NULL,
		source = NA,
		#' @details
		#' Create a new [Unit] object
		#' @param unit a unit of measurement
		#' @param @id identifier
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor
		#' @param source the source of the [OntologySource] object, was it listed in
 		#' @param unit_references A list of units used as a [UnitReferences] object
		initialize = function(
			unit = NULL,
			`@id` = character(),
			ontology_source_references = NULL,
			# unit_references = NULL,
			source = NA
		) {
			# self$set_unit_from_string(unit)
			# need a list of ontology annotations to account for
			# composit units e.g. ms^-2 is meters and seconds
			# need a separate field for the unit and the ontology
			# annotation(s) used in the unit
			#self$unit <- OntologyAnnotation$new(unit, OM)
			self$`@id` <- `@id`
			if(is.null(ontology_source_references)) {
				self$ontology_source_references <-
					OntologySourceReferences$new()
			} else {
				self$ontology_source_references <- ontology_source_references
			}
			if (is.null(unit)) {
				unit <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
			} else {
				self$unit <- unit
			}
			# self$unit_references <- unit_references
			self$source <- source
		},
		#' @details
		#' Get the term associated with the unit as a string
		#' @return a character vector of length one with the term of the unit
		get_unit_string = function() {
			self$unit$term
		},
		#' @details
		#' An R list representation of a [Unit] object
		#' @param recursive linked data (default FALSE)
		to_list = function(recursive = TRUE) {
			lst <- list()
			lst[["@id"]] <- self$`@id`
			if(recursive) {
				lst <- c(lst, self$unit$to_list())
			}
			return(lst)
		},
		to_table = function() {
			self$unit$to_table() %>% purrr::set_names(
				"Unit", "Term Source REF", "Term Accession Number"
				# paste0("Unit[",self$unit$term,"]"),
				# paste0("Term Source REF[",self$unit$term,"]"),
				# paste0("Term Accession Number[",self$unit$term,"]")
			)
		},
		#' @details
		#' Make [Unit] object from list
		#' @param lst an Unit object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, source = NA, add = FALSE) {
			self$`@id` <- lst[["@id"]]
			self$unit <- OntologyAnnotation$new(
				ontology_source_references = self$ontology_source_references
			)
			#self$unit <-
			self$unit$from_list(lst)
		},
		#' @details
		#' Pretty prints [Unit] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Unit"))
			green_bold_name_plain_content("@id", self[["@id"]])
			green_bold_name_plain_content("Type", self$unit$term)
			green_bold_name_plain_content("Source", self$source)
		}
	)
)

# metre <- Unit$new("metre")
# UnitAnnotation$new(1,metre)
