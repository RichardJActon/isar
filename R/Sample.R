#' R6 class for sample
#'
#' Represents a sample material in an experimental graph.
#'
#' @field name A name/reference for the sample material.
#' @field factor_values A list of [FactorValue]s used to qualify the material in terms of study factors/design.
#' @field study_factor_references a [StudyFactorReferences] object
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field derives_from A link to the source material that the sample is derived from.
#' @field comments Comments associated with instances of this class.
#' @field ontology_source_references an [OntologySourceReferences] object
#' @field category_references an [CharacteristicCategoryReferences] object
#' @field unit_references an [UnitReferences] object
#' @field sources list of available [Source]s
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_string test_r6
#' @importFrom cli cli_h1 cli col_blue
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#'
#' @export
Sample <- R6::R6Class(
	"Sample",
	# inherit = Material,
	public = list(
		name = character(),
		factor_values = NULL,
		study_factor_references = NULL,
		characteristics = NULL,
		derives_from = NULL,
		comments = NULL,
		ontology_source_references = NULL,
		category_references = NULL,
		unit_references = NULL,
		sources = NULL,
		`@id` = NULL,
		#' @details
		#'
		#' Create a new instance of sample
		#'
		#' @param name A name/reference for the sample material.
		#' @param factor_values A list of [FactorValue] objects used to qualify the material in terms of study factors/design.
		#' @param study_factor_references a [StudyFactorReferences] object
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param derives_from A link to the source material that the sample is derived from.
		#' @param comments Comments associated with instances of this class.
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param category_references an [CharacteristicCategoryReferences] object
		#' @param unit_references an [UnitReferences] object
		#' @param sources list of available [Source]s
		#' @param @id identifier
		initialize = function(
			name = character(),
			factor_values = NULL,
			study_factor_references = NULL,
			characteristics = NULL,
			derives_from = NULL,
			comments = NULL,
			ontology_source_references = NULL,
			category_references = NULL,
			unit_references = NULL,
			sources = NULL,
			`@id` = NULL
		) {
			if (checkmate::qtest(name, "S[0]")) {
				self$name <- name
			} else {
				self$set_name(name)
			}

			if (is.null(factor_values)) {
				self$factor_values <- factor_values
			} else {
				self$set_factor_values(factor_values)
			}
			self$study_factor_references <- study_factor_references
			self$characteristics <- characteristics # list
			self$derives_from <- derives_from # list

			self$ontology_source_references <- ontology_source_references
			self$category_references <- category_references
			self$unit_references <- unit_references
			self$sources <- sources
			self$comments <- comments
			self$`@id` <- `@id`
		},
		#' @details
		#' Check if the name of the material is a string
		#' @param name The name of the material
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the name of the material if valid
		#' @param name The name of the material
		set_name = function(name) {
			if (self$check_name(name)) { self$name <- name }
		},
		#' @details
		#'
		#' validates the factor_values field is a list of [FactorValue] objects
		#'
		#' @param factor_values factor values to be used in a [Sample] object
		#' A list of [FactorValue] objects
		check_factor_values = function(factor_values) {
			if(!is.list(factor_values)) {
				stop(glue::glue(
					.sep = "\n",
					"factor_values is not a list!",
					"factor_values must be a list for factor_value objects"
				))
			}
			is_factor_value <- purrr::map_lgl(
				factor_values, ~checkmate::test_r6(.x, "FactorValue")
			)
			if (all(is_factor_value)) {
				return(TRUE)
			} else {
				not_factor_values <- paste0(
					which(!is_factor_value), collapse = ", "
				)
				stop(glue::glue(
					.sep = "\n",
					"Not all factor_values are factor_value objects!",
					"Elements: {not_factor_values}",
					"are not factor values"
				))
			}
		},

		#' @details
		#'
		#' Sets the factor values used in the sample
		#'
		#' @param factor_values factor values used in the sample
		#' A list of [FactorValue] objects
		set_factor_values = function(factor_values) {
			if(self$check_factor_values(factor_values)) {
				self$factor_values <- factor_values
			}
		},

		# set_valid_factor_value = function(factor_values) {
		# 	factor_values %in% names(self$study_factor_references)
		# },

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' Sample$new()
		to_list = function(ld = FALSE) {
			lst <- list()
			# "id" = private$id
			lst[["@id"]] <- self$`@id`
			# lst[["name"]] <- self$name
			lst[["name"]] <- private$raw_name
			lst[["factorValues"]] <- self$factor_values %>%
				purrr::map(~.x$to_list()) %>%
				purrr::set_names(NULL)
			lst[["characteristics"]] <- purrr::map(
				self$characteristics, ~.x$to_list()
			)
			lst[["derivesFrom"]][[1]][["@id"]] <- self$derives_from$`@id` ## !!! potential multiple sources
			lst[["comments"]] <- self$comments
			return(lst)
		},

		#' @details
		#' generate a tabular representation of a sample object
		#' @return a Tibble
		to_table = function() {
			comments <- NULL
			if(!checkmate::test_list(self$comments, len = 0, null.ok = TRUE)) {
				comments <- self$comments %>% purrr::map_dfc(~{
					tibble::tibble_row(.x$value) %>%
						purrr::set_names(paste0("Comment[", .x$name, "]"))
				})
			}

			# factors <- self$factor_values %>%
			# 	purrr::map(~.x$to_table())

			if(checkmate::test_list(
				self$factor_values, len = 0, null.ok = TRUE
			)) {
				factors <- NULL
			} else {
				# factors <- factors %>% purrr::list_cbind()
				factor_categories <- self$factor_values %>%
					purrr::map_chr(~.x$`@id`)
				unique_factor_categories <- unique(factor_categories)
				factors <- unique_factor_categories %>% purrr::map(~{
					# factor_categories[factor_categories == .x]
					self$factor_values[which(factor_categories == .x)] %>%
						purrr::map(~.x$to_table()) %>%
						purrr::list_rbind()
				}) %>% purrr::list_cbind()
			}
			dplyr::bind_cols(
				comments,
				tibble::tibble("Sample Name" = self$name),
				factors
			)
		},

		#' @details
		#' Checks that source is listed in the provided list of sources
		#' @param source The name of a source to check is listed
		check_source = function(source) {
			if(source %in% names(self$sources)) { return(TRUE) } else {
				stop("source not listed!")
			}
		},
		#' @details
		#' Sets a source if that source is listed in the provided list of sources
		#' @param source The name of a source to set is listed
		set_source = function(source) {
			if(is.null(self$sources)) {
				self$derives_from$`@id` <- source
			} else{
				if(self$check_source(source)) {
					self$derives_from <- self$sources[[source]]
				}
			}
		},

		#' @details
		#' update characteristic categories with any new categories added to
		#' the reference.
		update_characteristics = function() {
			self$characteristics %>% purrr::walk(~{
				.x$set_category(self$category_references$categories[[.x$`@id`]])
			})
		},

		#' @details
		#' Make [sample] from list
		#' @param lst a list serialization of a [Sample] factor object
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			self$`@id` <- lst[["@id"]]
			# self$name <- lst[["name"]]
			private$raw_name <- lst[["name"]]
			self$name <- sub(".*?-(.*)", "\\1", private$raw_name)

			self$set_source(lst[["derivesFrom"]][[1]][["@id"]]) # multiple inputs? # are all always sources?
			# self$derives_from <- purrr::map(lst[["derivesFrom"]], ~{
			# 	if(.x %in% self$sources$ ) {
			# 		## self$references[["Source"]][[.x]]
			# 	} else {
			# 		warning("Unknown material!")
			# 		.x
			# 	}
			# })

			# self$derives_from <- lst[["derivesFrom"]]

			self$factor_values <-
				lst[["factorValues"]] %>%
				purrr::map(~{
					fv <- FactorValue$new(
						ontology_source_references =
							self$ontology_source_references,
						study_factor_references = self$study_factor_references,
						unit_references = self$unit_references
					)
					fv$from_list(.x, recursive = recursive, json = json)
					fv
				}) %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`))


			if (is.null(self$category_references)) {
				self$category_references <-
					CharacteristicCategoryReferences$new(
						ontology_source_references =
							self$ontology_source_references
					)
			}
			self$characteristics <- purrr::map(lst[["characteristics"]], ~{
				chr <- Characteristic$new(
					ontology_source_references =
						self$ontology_source_references,
					category_references = self$category_references,
					unit_references = self$unit_references
				)
				chr$from_list(.x, json = json, recursive = recursive)
				chr
			})
			# if (checkmate::test_list(lst[["characteristics"]], len = 0)) {
			# 	if(
			# 		!"UnknownCharacteristic" %in%
			# 		names(self$category_references$categories)
			# 	) {
			# 		self$category_references$add_categories(
			# 			list(
			# 				"UnknownCharacteristic" =
			# 				CharacteristicCategory$new(
			# 					`@id` = "Unspecified",
			# 					explicitly_provided = FALSE, source = self$name,
			# 					ontology_source_references =
			# 						self$ontology_source_references
			# 				)
			# 			)
			# 		)
			# 	}
			# 	self$characteristics <- list(Characteristic$new(
			# 		category = self$category_references$categories[[
			# 			"UnknownCharacteristic"
			# 		]],
			# 		#value = NA_integer_,
			# 		`@id` = "Unspecified",
			# 		ontology_source_references =
			# 			self$ontology_source_references,
			# 		category_references = self$category_references
			# 	))
			# } else {
			# 	self$characteristics <- purrr::map(lst[["characteristics"]], ~{
			# 		chr <- Characteristic$new(
			# 			ontology_source_references =
			# 				self$ontology_source_references,
			# 			category_references = self$category_references
			# 		)
			# 		chr$from_list(.x, json = json, recursive = recursive)
			# 		chr
			# 	})
			# }
			self$comments <- lst[["comments"]]
		},
		#' @details
		#' Pretty Prints [Sample] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Sample"))
			green_bold_name_plain_content("Name", self$name)
			# green_bold_name_plain_content("ID", private$id)
			green_bold_name_plain_content("Derives from", self$derives_from$name)
			cli::cli_h1(cli::col_green("Factor Categories"))
			cli::cli_ul(purrr::map_chr(self$factor_values, ~.x$`@id`))

			cli::cli_h1(cli::col_green("Characteristics"))
			purrr::walk(self$characteristics, ~{
				green_bold_name_plain_content(.x$category$type, .x$value$term)
			})

			pretty_print_comments(self$comments)
		}
	),
	private = list(
		raw_name = character()
	)
)

