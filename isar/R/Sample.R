#' R6 class for sample
#'
#' Represents a sample material in an experimental graph.
#'
#' @field name A name/reference for the sample material.
#' @field factor_values A list of [FactorValue]s used to qualify the material in terms of study factors/design.
#' @field characteristics A list of Characteristics used to qualify the material properties.
#' @field derives_from A link to the source material that the sample is derived from.
#' @field comments Comments associated with instances of this class.
#' @field ontology_source_references an [OntologySourceReferences] object
#' @field category_references an [CharacteristicCategoryReferences] object
#' @field unit_references an [UnitReferences] object
#' @field sources list of available [Source]s
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_string test_r6
#' @importFrom cli cli_h1 cli col_blue
#' @importFrom glue glue
#' @importFrom purrr map_lgl
#' @importFrom uuid UUIDgenerate
#'
#' @export
Sample <- R6::R6Class(
	"Sample",
	# inherit = Material,
	public = list(
		name = character(),
		factor_values = NULL,
		characteristics = NULL,
		derives_from = NULL,
		comments = NULL,
		ontology_source_references = NULL,
		category_references = NULL,
		unit_references = NULL,
		sources = NULL,
		#' @details
		#'
		#' Create a new instance of sample
		#'
		#' @param name A name/reference for the sample material.
		#' @param factor_values A list of [FactorValue] objects used to qualify the material in terms of study factors/design.
		#' @param characteristics A list of Characteristics used to qualify the material properties.
		#' @param derives_from A link to the source material that the sample is derived from.
		#' @param comments Comments associated with instances of this class.
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param category_references an [CharacteristicCategoryReferences] object
		#' @param unit_references an [UnitReferences] object
		#' @param sources list of available [Source]s
		initialize = function(
			name = character(),
			factor_values = NULL,
			characteristics = NULL,
			derives_from = NULL,
			comments = NULL,
			ontology_source_references = NULL,
			category_references = NULL,
			unit_references = NULL,
			sources = NULL
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
			self$characteristics <- characteristics # list
			self$derives_from <- derives_from # list

			self$ontology_source_references <- ontology_source_references
			self$category_references <- category_references
			self$unit_references <- unit_references
			self$sources <- sources
			self$comments <- comments
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

		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @examples
		#' Sample$new()
		to_list = function(ld = FALSE) {
			lst <- list()
			# "id" = private$id
			lst[["name"]] <- self$name
			lst[["factorValues"]] <- purrr::map(
				self$factor_values, ~.x$to_list()
			)
			lst[["characteristics"]] <- purrr::map(
				self$characteristics, ~.x$to_list()
			)
			lst[["derivesFrom"]][[1]][["@id"]] <- self$derives_from$`@id` ## !!! potential multiple sources
			lst[["comments"]] <- self$comments
			return(lst)
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
		#' Make [sample] from list
		#' @param lst a list serialization of a [Sample] factor object
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(!json) {
				private$id <- lst[["id"]]
			}
			self$name <- lst[["name"]]

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

			self$factor_values <- purrr::map(
				lst[["factorValues"]], ~{
					fv <- FactorValue$new(
						ontology_source_references =
							self$ontology_source_references
					)
					fv$from_list(.x, recursive = recursive, json = json)
					fv
				}
			)


			if (is.null(self$category_references)) {
				self$category_references <-
					CharacteristicCategoryReferences$new(
						ontology_source_references =
							self$ontology_source_references,
						unit_references = self$unit_references
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
			green_bold_name_plain_content("ID", private$id)
			green_bold_name_plain_content("Derives from", self$derives_from$name)
			cli::cli_h1(cli::col_green("Factor Categories"))
			cli::cli_ul(purrr::map_chr(self$factor_values, ~.x$`@id`))

			cli::cli_h1(cli::col_green("Characteristics"))
			purrr::walk(self$characteristics, ~{
				green_bold_name_plain_content(.x$category$type, .x$value$term)
			})

			pretty_print_comments(self$comments)
		}
	)# ,
	# private = list(
	# 	id = generate_id()
	# )
)

