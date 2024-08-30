# make value a private method, if it should only come from factor type?

#' R6 class for an experimental factor value
#'
#' @details
#' A factor_value represents the value instance of a [StudyFactor].
#'
#' @field factor an experimental factor
#' @field value value of a quantity
#' @field unit units in which that quantity is measured
#' @field ontology_source_references an [OntologySourceReferences] object
#' @field study_factor_references a [StudyFactorReferences] object
#' @field unit_references an [UnitReferences] object
#' @field comments comments
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
#'
#' @export
FactorValue <- R6::R6Class(
	"FactorValue",
	#inherit = StudyFactor,
	public = list(
		factor = NULL,
		value = NULL,
		unit = NULL,
		ontology_source_references = NULL,
		study_factor_references = NULL,
		unit_references = NULL,
		comments = NULL,
		`@id` =  character(),
		#' @details
		#' create a new factor value
		#' @param factor an experimental factor
		#' @param value the value of a quantity
		#' @param unit the unit of measurement
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param study_factor_references a [StudyFactorReferences] object
		#' @param unit_references an [UnitReferences] object
		#' @param comments comments
		#' @param @id identifier

		#'
		#' @examples
		#' \dontrun{
		#' FactorValue$new(
		#'     factor_name = sf,
		#'     value = "agrees with",
		#'     unit = NULL
		#' )
		#' }

		initialize = function(
			factor = NULL,
			value = NULL,
			unit = NULL,
			ontology_source_references = NULL,
			study_factor_references = NULL,
			unit_references = NULL,
			comments = NULL,
			`@id` = character()
		) {
			# if (!is.null(factor_name)) {
			# 	if (checkmate::check_r6(factor_name, "StudyFactor")) {
			# 		self$factor_name <- factor_name
			# 	}
			# 	self$value <- self$factor_name$factor_type
			# } else {
			# 	self$factor_name <-factor_name
			# 	self$value <- value
			# }
			# if (is.null(value)) {
			# 	self$value <- self$factor_name$factor_type
			# } else if (
			# 	checkmate::check_r6(value, "ontology_annotation") &&
			# 	value$term_accession == factor_name$factor_type$term_accession
			# ) {
			#
			# }

			self$factor <- factor
			# if(is.null(factor_name)) { self$value <- value }
			# if(
			# 	!is.null(factor_name) &&
			# 	value %in% names(
			# 		self$factor_name$factor_type$term_source$terms_list
			# 	)
			# ) {
			# 	self$value <- value
			# } else {
			# 	stop(glue::glue(
			# 		.sep = "\n",
			# 		"Value is not in a valid terms for this type of factor",
			# 		"see: factor_name$factor_type$term_source$terms_list"
			# 	))
			# }
			self$unit <- unit

			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references

			if(is.null(study_factor_references)) {
				self$study_factor_references <- StudyFactorReferences$new(
					ontology_source_references = self$ontology_source_references
				)
			} else {
				self$study_factor_references <- study_factor_references
			}

			self$comments <- comments
			self$`@id` <- `@id`
		},
		#' @details
		#' check if unit is a [Unit] object
		#' @param unit a [Unit] object
		check_unit = function(unit) {
			check <- checkmate::check_r6(unit, "Unit")
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set unit if input is valid
		#' @param unit a [Unit] object
		set_unit = function(unit) {
			if(self$check_unit(unit)) { self$unit <- unit }
		},
		#' @details
		#' checks if comments are a named list of character vectors
		#' @param comments comments
		check_comments = function(comments) { check_comments(comments) },
		#' @details
		#' Sets comments if they are in a valid format
		#' @param comments a list of comments
		set_comments = function(comments) {
			if(self$check_comments(comments)) { self$comments <- comments }
		},
		#' @details
		#' Add comment if it is in a valid format
		#' @param comment a list of comments
		add_comment = function(comment) {
			if(self$check_comments(comment)) {
				self$comments <- c(comments, comment)
			}
		},
		#' @details
		#' set an experimental factor of a valid category
		#' @param factor an experimental factor
		set_valid_factor_category = function(factor) {
			#browser()
			if(is.null(self$study_factor_references)) {
				self$study_factor_references <- StudyFactorReferences$new(
					ontology_source_references =
						self$ontology_source_references,
					unit_references = self$unit_references

				)
			}
			if(
				factor$`@id` %in%
				self$study_factor_references$get_study_factor_ids()
			) {
				self$factor <-
					self$study_factor_references$study_factor_references[[
						factor$`@id`
					]]
			} else {
				warning("Factor category not listed!")
				sf <- StudyFactor$new(
					factor_type = self$factor,# explicitly_provided = FALSE
					origin = self$`@id`
				)
				self$study_factor_references$add_study_factors(
					list("UndefinedFactor" = sf)
				)
			}
		},
		set_valid_unit = function(lst) {
			unit_id <- lst[["@id"]]
			if(is.null(self$unit_references)) {
				warning(
					"No unit references supplied!",
					"Creating and empty reference..."
				)
				self$unit_references <- UnitReferences$new(
					ontology_source_references =
						self$ontology_source_references
				)
			}
			if (unit_id %in% self$unit_references$get_unit_ids()) {
				self$unit <- self$unit_references$units[[unit_id]]
			} else {
				self$unit <- Unit$new(
					ontology_source_references =
						self$ontology_source_references
				)
				self$unit$from_list(lst)
				self$unit %>% list() %>% purrr::set_names(unit_id) %>%
					self$unit_references$add_unit_references()
			}
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		to_list = function(ld = FALSE) {
			lst <- list()
			#"factor_name" = self$factor_name$to_list(),
			#"factor_name" = super$factor_name$to_list(),
			if(is.null(self$unit)) {
				lst[["value"]] <- self$value$to_list()
			} else {
				lst[["value"]] <- self$value
				lst[["unit"]] <- self$unit$to_list(recursive = FALSE)
			}
			lst[["comments"]] <- self$comments
			#"@id" = super$`@id`
			lst[["category"]][["@id"]] <- self$`@id`
			return(lst)
		},

		to_table = function() {
			tbl <- NULL
			if(checkmate::test_r6(self$value, "OntologyAnnotation")) {
				tbl <- self$value$to_table() %>%
					purrr::set_names(
						paste0("Factor Value[", self$factor$factor_name , "]"),
						paste0(
							"Term Source REF[", self$factor$factor_name, "]"
						),
						paste0(
							"Term Accession Number[", self$factor$factor_name,
							"]"
						)
					)
			} else if(checkmate::test_r6(self$unit, "Unit")) {
				tbl <- tibble::tibble_row(self$value) %>%
					purrr::set_names(
						"Factor Value"
						#paste0("Factor Value[", self$`@id`, "]")
					) %>% dplyr::bind_cols(self$unit$to_table())
				colnames(tbl) <- paste0(
					colnames(tbl), "[", self$factor$factor_name, "]"
				)
			}
			return(tbl)
		},
		#' @details
		#'
		#' Make [OntologyAnnotation] from list
		#'
		#' @param lst an ontology source object serialized to a list
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		#' @param json json  (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				#self$set_id()
				# self$factor_name <- StudyFactor$new()
				# self$factor_name$from_list(
				# 	lst[["factor_name"]], recursive = recursive, json = json
				# )
				self$`@id` <- lst[["category"]][["@id"]]
				#self$factor <- super$get_or_create_reference(
				# "StudyFactor", lst[["category"]][["@id"]],
				# value = lst[["factor_name"]]
				# )
				if (!is.null(lst[["unit"]])) {
					self$set_valid_unit(lst[["unit"]])
				}
				# self$factor <-
				self$set_valid_factor_category(
					lst[["category"]]#[["@id"]],
					#lst[["factor_name"]]
				)
				# if reference exists reference it - else make one
				# self$factor_name$name <- sub(
				# 	"#factor/", "", self$`@id`, fixed = TRUE
				# )
				#self$factor_name <- self$factor$factor_name
				if(is.list(lst[["value"]])) {
					self$value <- OntologyAnnotation$new(
						ontology_source_references =
							self$ontology_source_references
					)
					self$value$from_list(
						lst[["value"]], recursive = recursive, json = json
					)
				} else {
					self$value <- lst[["value"]] %>% as.numeric()
				}
				self$set_comments(lst[["comments"]])
			} else {
				self$factor_name <- StudyFactor$new()
				self$factor_name$from_list(lst[["factor_name"]])
				# self$value <- self$factor_name$factor_type
				# !! not using value direct from list but from factor ~
				self$value <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
				self$value$from_list(lst[["value"]])
				self$unit <- lst[["unit"]]
				self$comments <- lst[["comments"]]
			}
		},
		#' @details
		#' Pretty prints [FactorValue] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Factor Value"))
			#green_bold_name_plain_content("Name", self$factor_name$name)
			if (!is.null(self$unit)) {
				green_bold_name_plain_content("Value", self$value)
				green_bold_name_plain_content("Unit ID", self$unit$`@id`)
			} else {
				green_bold_name_plain_content("Value", self$value$term)
			}
			green_bold_name_plain_content("@id", self$`@id`)
			#green_bold_name_plain_content("ID", private$id)
			pretty_print_comments(self$comments)
		}
	)
)
