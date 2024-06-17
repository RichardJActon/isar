#' R6 class for OntologySourceReferences
#'
#' @field ontology_source_references ...
#'
#' @importFrom R6 R6Class
#'
#' @export
OntologySourceReferences <- R6::R6Class(
	"OntologySourceReferences",
	public = list(
		ontology_source_references = NULL,
		initialize = function(ontology_source_references = NULL) {
			self$ontology_source_references <- ontology_source_references
		},
		#' @details
		#' check ontology_sources is a list of [OntologySource] objects
		#' @param ontology_sources a list of [OntologySource] objects
		check_ontology_sources = function(ontology_sources) {
			if(
				checkmate::test_list(
					ontology_sources, min.len = 1, names = "unique"
				) && all(purrr::map_lgl(
					ontology_sources, ~checkmate::test_r6(.x, "OntologySource")
				))
			) { return(TRUE) } else {
				stop("All ontology sources must be OntologySource objects")
			}
		},
		#' @details
		#' Add ontology sources to the
		#' @return a vector of ontology source names
		add_ontology_sources = function(ontology_sources) {
			if(self$check_ontology_sources(
				c(self$ontology_source_references, ontology_sources)
			)) {
				self$ontology_source_references <- c(
					self$ontology_source_references, ontology_sources
				)
			}
		},
		#' @details
		#' Get the names of the Ontology Sources Used in this Investigation
		#' @return a vector of ontology source names
		get_ontology_source_names = function() {
			names(self$ontology_source_references)
		},

		#' @details
		#' Get the provision status of Ontology Sources, where they explicitly
		#' listed in the Investigation or not?
		#' @return a vector of ontology source names
		get_ontology_source_provision = function() {
			self$ontology_source_references %>%
				purrr::map_lgl(~.x$explicitly_provided) %>%
				purrr::set_names(names(self$ontology_source_references))
		},

		#' @details
		#' Get ontology sources
		#' @param names default return all names
		#' @return a vector of ontology source names
		get_ontology_sources = function(names = "all") {
			if(names == "all") {
				self$ontology_source_references
			} else {
				self$ontology_source_references[[names]]
			}
		},

		# list of objects from which ontology source objects originate
		# there should ideally only be the ontology sources explicitly
		# listed in the investigation but if any other have been used
		# without being specified or an ontology is missing where one should
		# be used filler sources will be generated which will be listed as
		# source references
		get_ontology_source_origins = function() {
			purrr::map_chr(self$ontology_source_references, ~.x$source)
		},

		to_list = function(source = "any") {
			if(source == "any") {
				self$ontology_source_references %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else if (source %in% self$get_ontology_source_origins()) {
				self$ontology_source_references %>%
					`[`(self$get_ontology_source_origins() %in% source) %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else {
				possible_values <- self$get_ontology_source_origins() %>%
					unique() %>%
					paste0(collapse = ", ")
				stop(paste0("source must be one of: any, ", possible_values))
			}

		},

		#' @details
		#'
		#' Make [OntologySourceReferences] from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst, explicitly_provided = logical(), source = NA) {#, json = TRUE
		# add_from_list = function(lst) {#, json = TRUE
			ontology_sources <- purrr::map(
				lst, ~{
					os <- OntologySource$new(
						explicitly_provided = explicitly_provided,
						source = source
					)
					os$from_list(.x, json = TRUE)
					os
				}
			)
			# possible problems with name collisions and different versions
			# of the same ontology! - use a unique id instead
			names(ontology_sources) <- purrr::map_chr(ontology_sources, ~.x$name)
			self$ontology_source_references <- ontology_sources
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Ontology Source References"))
			purrr::walk(self$ontology_source_references, ~.x$print())
		}
	)
)

# osr <- OntologySourceReferences$new()
# osr$from_list(BII_I_1_jsonlite[["ontologySourceReferences"]])
# osr$ontology_source_references
