# setup
# BII_I_1_tab <- readr::read_tsv("../data/ISAdatasets/tab/BII-I-1/s_BII-S-1.txt")
# BII_S_3_tab <- readr::read_tsv("../data/ISAdatasets/tab/BII-S-3/s_BII-S-3.txt")
# library(devtools)
# load_all()

#' enumerate_roles
#'
#' @param persons list of persons from isa json read in by jsonlite
#'
#' @return a list of roles in the form of a 3 element list with names
#' termAccession termSource annotationValue, the components of an OntologyAnnotation
#' @export
#'
enumerate_roles <- function(persons) {
	purrr::map(persons, ~{ unlist(.x[["roles"]], recursive = FALSE) })
}

# enumerate_roles(BII_I_1_jsonlite[["people"]])

#' enumerate_all_roles
#'
#' @param isa_json isa json read in by jsonlite
#'
#' @return a list of roles in the form of a 3 element list with names
#' termAccession termSource annotationValue, the components of an OntologyAnnotation
#' @export
#'
enumerate_all_roles <- function(isa_json) {
	roles <- list()
	if(!is.null(isa_json[["people"]])) {
		roles <- c(roles, enumerate_roles(isa_json[["people"]]))
	}
	if(!is.null(isa_json[["studies"]])) {
		roles <- c(
			roles, unlist(
				purrr::map(isa_json[["studies"]], ~{
					x <- .x[["people"]]
					enumerate_roles(x)
				}),
				recursive = FALSE
			)
		)
	}
	return(roles)
}
# Example sources of person roles from different places:
# BII_I_1_jsonlite[["people"]][[1]][["roles"]]
# BII_I_1_jsonlite[["studies"]][[1]][["people"]][[1]][["roles"]]
#
# roles <- enumerate_all_roles(BII_I_1_jsonlite)

#' unique_role_sources
#'
#' @param roles list of roles
#'
#' @return vector of unique role sources
#' @export
#'
unique_role_sources <- function(roles) {
	unique(purrr::map_chr(roles, ~.x$termSource))
}

# role_sources(enumerate_roles(BII_I_1_jsonlite[["people"]]))

#' generate_ontology_source_from_annotation_with_undefined_source
#'
#' takes a list of ontology annotation information in the form of
#' a 3 element list with names termAccession termSource annotationValue,
#' the components of an OntologyAnnotation but with the source undefined.
#' Creates a placeholder [OntologySource] object for all the annotationValues
#' in this list where the source is undefined. Accesssion is ignored as it
#' is meaningless in the absence of a source
#'
#' If the input items with undefined [OntologySource]s are of the same type
#' this provides a quick way to make a new OntologySource object for them
#'
#' @param annotation_list list of 3 elements lists with the names
#' termAccession termSource annotationValue
#' @param source_name The name for the [OntologySource] DEFAULT UndefinedSource
#' @param source_description The description of the [OntologySource] DEFAULT undefined source
#' @param ... other arguments to `OntologySource$new()`
#'
#' @return an [OntologySource] object or NULL if there are no undefined sources
#' @export
#'
generate_ontology_source_from_annotation_with_undefined_source <- function(
	annotation_list,
	source_name = "UndefinedSource", source_description = "undefined source",
	...
) {
	sources_lgl <- purrr::map_lgl(annotation_list, ~{
		checkmate::qtest(.x, "S[0]")
	})
	if(any(!sources_lgl)) {
		annotations_with_no_source <- annotation_list[!sources_lgl]
		os <- OntologySource$new(
			name = source_name, description = source_description,
			terms_list = c(
				purrr::map_chr(
					annotations_with_no_source, ~{.x$annotationValue}
				) %>% unique() %>% purrr::set_names(),
				list("undefined" = "undefined")
			),
			...
		)
		return(os)
	} else (
		return(NULL)
	)
}
# generate_ontology_source_from_annotation_with_undefined_source(
# 	roles, "RolesOfUndefinedSource",
# 	"Roles for Persons which lacked a source for the term"
# 	)

# roles_to_ontology_sources <- function(roles, investigation) {
# 	sources_lgl <- purrr::map_lgl(roles, ~{checkmate::qtest(.x, "S[0]")})
# 	roles_with_source <- roles[sources_lgl]
# 	roles_with_no_source <- roles[!sources_lgl]
#
# 	investigation$get_ontology_source_names()
#
# 	if(roles_with_source) {
# 		sources_lgl
# 	}
# }

#' ontology_annotation_with_undefined_source_handler
#'
#' @param self object
#' @param lst ontology anotation components list
#' @param member the member of the object being evaluated
#' @param ontology_source_name the name of the auto-generated OntologySource
#' @param source_description the description of the auto-generated OntologySource
#'
#' @return NULL
#' @export
#'
ontology_annotation_with_undefined_source_handler <- function(
		self, lst, member, ontology_source_name = "UnknownOntologySource",
		source_description
){

	# Should this be a method of OntologySourceReferences?
	# approach to merging autogenerated ontologies for the same types of thing?

	sources <- purrr::map_chr(lst, ~.x$termSource)

	undefined_lgl <- sources == ""
	if(any(undefined_lgl)) {
		warning(
			"Found ", member, " with undefined ontology sources!\n",
			"Creating a placeholder OntologySource for undefined sources"
		)
		UnknownOntologySource <- generate_ontology_source_from_annotation_with_undefined_source(
			lst[undefined_lgl],
			source_name = ontology_source_name,
			source_description = source_description,
			comments = list(
				"isar autogenerated" =
				"a source was undefined this was generated as a result"
			)
		)
		UnknownOntologySource %>% list() %>%
			purrr::set_names(ontology_source_name) %>%
			self$add_ontology_sources()

		lst[undefined_lgl] %>% purrr::walk(~{
			self[[member]] <- c(self[[member]], OntologyAnnotation$new(
				term = .x$annotationValue,
				term_source = self$ontology_source_references[[
					ontology_source_name
				]],
				term_accession = .x$termAccession
			))
		})
	}

	sources_avaliable_lgl <-
		sources %in% self$get_ontology_source_names()
	if(any(sources_avaliable_lgl)) {
		lst[sources_avaliable_lgl] %>% purrr::walk(~{
			self[[member]] <- c(self[[member]], OntologyAnnotation$new(
				term = .x$annotationValue,
				term_source = self$ontology_source_references[[
					.x$termSource
				]],
				term_accession = .x$termAccession
			))
		})
	}

	undefined_and_unavailable_lgl <-
		!sources_avaliable_lgl & !undefined_lgl
	if(any(undefined_and_unavailable_lgl)) {
		warning(
			"Found ", member, " with specified ontology sources that are",
			" not defined in OntologySourceReferences!\n",
			"Creating a placeholder OntologySource for missing sources"
		)
		lst[undefined_and_unavailable_lgl] %>%
			purrr::walk(~{
				os <- OntologySource$new(
					name = .x$termSource,
					terms_list = list(.x$termAccession) %>%
						purrr::set_names(.x$annotationValue),
					comments = list(
						"isar autogenerated" =
						"a source was missing this was generated as a result"
					)
				)
				os %>%
					list() %>%
					purrr::set_names(.x$termSource) %>% # member
					self$add_ontology_sources()
				self[[member]] <- OntologyAnnotation$new(
					term = .x$annotationValue,
					term_source = self$ontology_source_references[[
						.x$termSource
					]],
					term_accession = .x$termAccession
				)
			})
	}
}


# Investigation ----
# inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = FALSE, json = TRUE)
# inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)

# Person ----
# ~~complete~~
# p1 <- Person$new()
# p1$from_list(BII_I_1_jsonlite$people[[1]], json = TRUE)

# Publications ----
# ~~complete~~
# BII_I_1_jsonlite$publications[[1]]
# pub1 <- Publication$new()
# pub1$from_list(BII_I_1_jsonlite$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[1]]$publications[[1]], recursive = TRUE, json = TRUE)
# pub1$from_list(BII_I_1_jsonlite$studies[[2]]$publications[[1]], recursive = TRUE, json = TRUE)

# Studies
# BII_I_1_jsonlite$studies[[1]]
# s1 <- Study$new()
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = FALSE, json = TRUE)
# s1$from_list(BII_I_1_jsonlite$studies[[1]], recursive = TRUE, json = TRUE)
# s1

# Factors ~~complete~~
# BII_I_1_jsonlite$studies[[1]]$factors[[1]]
# sf1 <- StudyFactor$new()
# sf1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]],json = TRUE,recursive = FALSE)
# sf1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]],json = TRUE,recursive = TRUE)
# sf1


# ontologySourceReferences
# BII_I_1_jsonlite$ontologySourceReferences[[1]]
# os1 <- OntologySource$new()
# os1$from_list(BII_I_1_jsonlite$ontologySourceReferences[[1]], json = TRUE)

# BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType
# oa1 <- OntologyAnnotation$new()
# oa1$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]]$factorType, json = TRUE)


# Datafile
# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFilesi[[1]]
# df <- DataFile$new()
# df$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$dataFiles[[1]])

# Measurement type
# mt <- OntologyAnnotation$new()
# mt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$measurementType, recursive = TRUE, json = TRUE)


# tt <- OntologyAnnotation$new()
# tt$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$technologyType, recursive = TRUE, json = TRUE)


# Characteristics
# BII_I_1_jsonlite$studies[[1]]$characteristicCategories[[1]]#$characteristicType
# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$characteristicCategories[[1]]
# BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]$characteristics[[1]]#$value
# BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$characteristics[[1]]


# ct1 <- Characteristic$new()
# ct1$from_list(BII_I_1_jsonlite$studies[[1]]$assays[[1]]$characteristicCategories[[1]], json = TRUE)
# ct1
# ct1$value
# ct2 <- Characteristic$new()
# ct2$from_list(BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]$characteristics[[1]], json = TRUE)
# ct2
# ct2$value

# ctoa1 <- OntologyAnnotation$new()
# ctoa1$from_list() BII_I_1_jsonlite$studies[[1]]$characteristicCategories[[1]]$characteristicType

# protocols
# pc <- Protocol$new()
# pc$from_list(BII_I_1_jsonlite$studies[[1]]$protocols[[2]], recursive = TRUE, json = TRUE)
# pc$from_list(BII_I_1_jsonlite$studies[[1]]$protocols[[6]], recursive = TRUE, json = TRUE)
# pc

# process
# pr <- Process$new()
# pr$from_list(BII_I_1_jsonlite$studies[[1]]$processSequence[[1]], recursive = FALSE, json = TRUE)
# pr

# uoa <- OntologyAnnotation$new()
# uoa$from_list(BII_I_1_jsonlite$studies[[1]]$unitCategories[[1]], recursive = TRUE, json = TRUE)
# uoa

# Sources
# src <- Source$new()
# src$from_list(BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]], recursive = TRUE, json = TRUE)
# src

# Samples
# samp <- Sample$new()
# samp$from_list(BII_I_1_jsonlite$studies[[1]]$materials$samples)
# samp

# BII_I_1_jsonlite$studies[[1]]$assays[[1]]$technologyType
# BII_I_1_jsonlite$studies[[1]]$unitCategories[[1]]

# BII_S_3_jsonlite$studies[[1]]$processSequence[[1]]$executesProtocol$`@id`
# BII_S_3_jsonlite$studies[[1]]$processSequence[[1]]$`@id`


# Factor value seems parsable as a characteristic? - what's the value of a dedicated object?
# fvac <- Characteristic$new()
# fvac$from_list(
# 	BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[1]],
# 	recursive = TRUE, json = TRUE
# )
# fvac

# sf <- StudyFactor$new()
# sf$from_list(BII_I_1_jsonlite$studies[[1]]$factors[[1]], recursive = TRUE, json = TRUE)
# sf

# fv <- FactorValue$new()
# fv$from_list(BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[1]], recursive = TRUE, json = TRUE)
# fv
