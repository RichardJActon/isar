#' CharacteristicCategoryReferences
#'
#' @field categories A list of [CharacteristicCategory] objects
#' @field ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map_lgl map_chr map set_names walk
#' @importFrom checkmate check_r6
#' @importFrom cli cli_h1 col_blue
#'
#' @export
CharacteristicCategoryReferences <- R6::R6Class(
	"CharacteristicCategoryReferences",
	public = list(
		categories = list(),
		ontology_source_references = NULL,
		# source = character(),
		#' @details
		#' Create a new [CharacteristicCategoryReferences] object
		#' @param categories characteristic categories
		#' @param ontology_source_references an [OntologySourceReferences] object listing all the ontology sources used
		initialize = function(
			categories = list(),
			ontology_source_references = NULL#,
			#source = character()
		) {
			self$categories <- categories
			self$ontology_source_references <- ontology_source_references
			#self$source <- source
		},
		#' @details
		#' Check if categories are [CharacteristicCategory] references
		#' @param categories characteristic categories
		check_categories = function(categories) {
			if(all(purrr::map_lgl(categories, ~checkmate::check_r6(
				.x, "CharacteristicCategory"
			)))) { return(TRUE) } else {
				stop("All categories must be CharacteristicCategory objects!")
			}
		},
		#' @details
		#' Add categories to the reference an check that they are
		#' [CharacteristicCategory] objects
		#' @param categories characteristic categories
		add_categories = function(categories) {
			if(self$check_categories(categories)) {
				self$categories <- c(self$categories, categories)
			}
		},
		#' @details
		#' Set the categories in the characteristic category reference,
		#' overwrites existing categories
		#' @param categories Characterisic categories
		set_categories = function(categories) {
			if(self$check_categories(categories)) {
				self$categories <- categories
			}
		},

		#' @details
		#' Get characteristic categories
		#' @param names default return all names
		#' @return a vector of [CharacteristicCategory] objects
		get_characterisic_categories = function(names = "all") {
			if(names == "all") { self$categories } else {
				self$categories[[names]]
			}
		},

		#' @details
		#' Checks that the type is [CharacteristicCategory]
		#' @param type category type
		check_type = function(type) {
			check <- checkmate::check_r6(
				type, "CharacteristicCategory", null.ok = TRUE
			)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the type value
		#' @param type category type
		set_type = function(type) {
			if(self$check_type(type)) { self$type <- type }
		},
		#' @details
		#' Get the ids of the categories in the reference
		#' @return character vector of category names
		get_category_ids = function() {
			names(self$categories)
		},
		#' @details
		#' Get the names of the categories in the reference
		#' @return character vector
		get_category_names = function() {
			purrr::map_chr(self$categories, ~.x$type$term)
		},

		#' @details
		#' get the source of the characteristic category, such as from which
		#' study does it originate
		#' @return character vector of characteristic category sources
		get_characteristic_category_origins = function() {
			purrr::map_chr(self$categories, ~.x$source)
		},

		#' @details
		#' Serialize [CharacteristicCategoryReferences] object to an R list
		#' @param source ids of sources of characteristic categories for which
		#' to generate a list of category references. 
		#' "any" lists categories from all sources. 
		#' default = "any"
		#' @return an R list
		to_list = function(source = "any") {
			if(source == "any") {
				self$categories %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else if(source %in% self$get_characteristic_category_origins()) {
				self$categories %>%
					`[`(
						self$get_characteristic_category_origins() %in% source
					) %>%
					purrr::map(~.x$to_list()) %>%
					purrr::set_names(NULL)
			} else {
				list()
				# possible_values <-
				# 	self$get_characteristic_category_origins() %>%
				# 	unique() %>%
				# 	paste0(collapse = ", ")
				# stop(paste0("source must be one of: any, ", possible_values))
			}
		},
		#' @details
		#' Populate [CharaceristicCategoryReferences] object from a list
		#' @param lst a list from which to generate a [CharaceristicCategoryReferences] object
		#' @param explicitly_provided (logical) if true the category was
		#' explicitly declared if false it was generated automatically as no
		#' explicitly declared category was found
		#' @param source the id of the object in which this category originated
		#' @param add (logical) if true append new charateristic categoeries to
		#' the ones already present in the reference, if fale then overwrite and
		#' existing ones.
		from_list = function(
			lst, explicitly_provided = logical(), source = NA, add = FALSE
		) {
			ccl <- lst %>%
				purrr::map(~{
					cc <- CharacteristicCategory$new(
						explicitly_provided = explicitly_provided,
						source = source,
						ontology_source_references =
							self$ontology_source_references
					)
					cc$from_list(.x)
					cc
				}) %>%
				purrr::set_names(., purrr::map_chr(., ~.x[["@id"]]))

			if(add) {
				nmslgl <- names(ccl) %in% self$get_category_ids()
				if(any(nmslgl)) {
					warning(
						"Overwriting a characterictic category with an identical name!",
						"Suggests characteristic categories did not list all categories used in a study."
					)
					nms <- names(ccl)[nmslgl]
					self$categories[nms] <- ccl[nmslgl]
					ccl <- ccl[-nmslgl]
				}
				self$add_categories(ccl)
			} else {
				self$set_categories(ccl)
			}
		},
		#' @details
		#' Pretty prints [CharacteristicCategoryReferences] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Characteristic Category References"))
			purrr::walk(self$categories, ~.x$print())
		}
	)
)
