#' R6 class for OntologySourceReferences
#'
#' @field ontology_source_references a list of [OntologySource] objects
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate test_list test_r6
#' @importFrom purrr map_lgl set_names map_chr map walk
#' @importFrom cli cli_h1 col_blue
#'
#' @export
OntologySourceReferences <- R6::R6Class(
	"OntologySourceReferences",
	public = list(
		ontology_source_references = list(),
		#' @details
		#' Create a new OntologySourceReferences object
		#' @param ontology_source_references a list of [OntologySource] objects
		initialize = function(ontology_source_references = list()) {
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
		#' @param ontology_sources list of [OntologySource] objects
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
		#' Add ontology source to the reference
		#' @param ontology_source an [OntologySource] object
		add_ontology_source = function(ontology_source) {
			if (checkmate::test_r6(ontology_source, "OntologySource")) {
				ontology_source %>%
					list() %>%
					purrr::set_names(ontology_source$name) %>%
					self$add_ontology_sources()
			} else {
				stop("ontology_source is not an OntologySource object!")
			}
		},
		#' @details
		#' Get the names of the Ontology Sources Used in this Investigation
		#' @return a vector of ontology source names
		get_ontology_source_names = function() {
			names(self$ontology_source_references)
		},

		#' @details
		#' Get ontology sources
		#' @param names default return all names
		#' @return a vector of ontology source objects
		get_ontology_sources = function(names = "all") {
			if(names == "all") {
				self$ontology_source_references
			} else {
				self$ontology_source_references[[names]]
			}
		},

		#' @details
		#' list of objects from which ontology source objects originate
		#' there should ideally only be the ontology sources explicitly
		#' listed in the investigation but if any other have been used
		#' without being specified or an ontology is missing where one should
		#' be used filler sources will be generated which will be listed as
		#' source references
		#' @return character vector of the origins of the [OntologySource]
		#' objects in the reference
		get_ontology_source_origins = function() {
			purrr::map_chr(self$ontology_source_references, ~.x$source)
		},

		#' @details
		#' Generate tabular representation of reference ontology sources
		#' @return a Tibble
		to_table = function() {
			tbl <- self$ontology_source_references %>%
				purrr::map_dfr(~{tibble::tibble(
					"Term Source Name" = .x$name,
					"Term Source File" = .x$file,
					"Term Source Version" = .x$version,
					"Term Source Description" = .x$description
				)}) %>%
				t() %>%
				as.data.frame() %>%
				tibble::rownames_to_column() %>%
				tibble::as_tibble()

			colnames(tbl)[-1] <- tbl %>%
				dplyr::slice(1) %>% unlist() %>% `[`(-1)

			tbl
		},

		#' @details
		#' Serialise a tabular representation of reference ontology sources
		#' 
		#' @param path path/name of the file to which to write the table
		# # overwrite ?
		cat_table = function(path = stdout()) {
			cat(file = path, "ONTOLOGY SOURCE REFERENCE\n", append = TRUE)
			self$to_table() %>%
				dplyr::mutate(dplyr::across(
					-rowname, ~paste0('"', .x, '"')
				)) %>%
				readr::write_tsv(
					file = path, quote = "none",  #quote = "needed",
					append = TRUE, na = "", escape = "none",
					col_names = FALSE, progress = FALSE
				)
		},

		#' @details
		#' Serialize object to a list
		#' @param source ontology sources of which origin to include
		#' if any includes all sources including those not explicitly provided
		#' (default: "any")
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
		#' @param explicitly_provided was the source explicitly provided
		#' as opposed to be automatically generated.
		#' @param source the origin of the  (default: NA)
		from_list = function(
			lst, explicitly_provided = logical(), source = NA
		) {#, json = TRUE
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
			names(ontology_sources) <- purrr::map_chr(
				ontology_sources, ~.x$name
			)
			self$ontology_source_references <- ontology_sources
		},
		#' @details
		#' Pretty prints an [OntologySourceReferences] object
		print = function() {
			cli::cli_h1(cli::col_blue("Ontology Source References"))
			purrr::walk(self$ontology_source_references, ~.x$print())
		}
	)
)

# osr <- OntologySourceReferences$new()
# osr$from_list(BII_I_1_jsonlite[["ontologySourceReferences"]])
# osr$ontology_source_references

#' check_ontology_source_references
#' 
#' returns TRUE if ontology_source_references is an [OntologySourceReferences]
#' object and throws an error if it is not
#'
#' @param ontology_source_references something you want to check is an
#' [OntologySourceReferences] object.
#'
#' @export
#'
check_ontology_source_references <- function(ontology_source_references) {
	check <- checkmate::check_r6(
		ontology_source_references, "OntologySourceReferences"
	)
	error_with_check_message_on_failure(check)
}

#' set_ontology_source_references
#' 
#' sets ontology_source_references attribute if 
#' ontology_source_reference is an [OntologySourceReferences] object
#'
#' @param self an object with an ontology_source_references attribute 
#' @param ontology_source_references an [OntologySourceReferences] object
#' @param null.action how to handle NULLs:
#' - "error" thow an error 
#' - "passthrough" set to NULL
#' - "create" set to an empty  [OntologySourceReferences] object
set_ontology_source_references <- function(
	self, ontology_source_references, null.action = "error"
) {
	if(is.null(ontology_source_references)) {
		switch(null.action,
			"error" = {
				stop("ontology_source_references must not be NULL!")
			},
			"passthrough" = {
				self$ontology_source_references <- 
					ontology_source_references
			},
			"create" = {
				self$ontology_source_references <- 
					OntologySourceReferences$new()
			}
		)
	} else if (
		check_ontology_source_references(ontology_source_references)
	) { self$ontology_source_references <- ontology_source_references }
}