#' R6 class for OntologySourceReferences
#'
#' @details
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
		#'
		#' Make [OntologySourceReferences] from list, defines the set of
		#' Ontology Sources which should be exhaustive for an investigation
		#'
		#' @param lst an ontology source object serialized to a list
		predefined_from_list = function(lst) {#, json = TRUE
			self$ontology_source_references <- purrr::map(
				lst, ~{
					os <- OntologySource$new()
					os$from_list(.x, json = TRUE)
					os
				}
			)
		},
		#' @details
		#'
		#' Make [OntologySourceReferences] from list
		#'
		#' @param lst an ontology source object serialized to a list
		from_list = function(lst) {#, json = TRUE
		# add_from_list = function(lst) {#, json = TRUE
			ontology_sources <- purrr::map(
				lst, ~{
					os <- OntologySource$new()
					os$from_list(.x, json = TRUE)
					os
				}
			)

			self$ontology_source_references <- ontology_sources
		},
		#' @details
		#' check ontology_sources is a list of [OntologySource] objects
		#' @param ontology_sources a list of [OntologySource] objects
		check_ontology_sources = function(ontology_sources) {
			if(
				checkmate::test_list(ontology_sources, min.len = 1) &&
				all(purrr::map_lgl(
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
			if(check_ontology_sources(ontology_sources)) {
				self$ontology_source_references <- c(
					self$ontology_source_references, ontology_sources
				)
			}
		},
		#' @details
		#' Get the names of the Ontology Sources Used in this Investigation
		#' @return a vector of ontology source names
		get_ontology_source_names = function() {
			purrr::map_chr(self$ontology_source_references, ~.x$name)
		}
	)
)

# osr <- OntologySourceReferences$new()
# osr$from_list(BII_I_1_jsonlite[["ontologySourceReferences"]])
# osr$ontology_source_references
