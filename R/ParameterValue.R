#' An R6 object for representing ParameterValue
#' A ParameterValue represents the instance value of a [ProtocolParameter], used in a Process.
#' @field category A link to the relevant [ProtocolParameter] that the value is set for.
#' @field value The value of the parameter.
#' @field unit The qualifying unit classifier, if the value is numeric.
#' @field ontology_source_references ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
#' @field unit_references A list of units used as a [UnitReferences] object
#' @field protocol_parameters a named list of available [ProtocolParameter] objects
#' @field comments Comments associated with instances of this class.
# #' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
#' @importFrom uuid UUIDgenerate
ParameterValue <- R6::R6Class(
	"ParameterValue",
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		protocol_parameters = NULL,
		comments = NULL,
		#`@id` = character(),
		#' @details
		#' New [ParameterValue] object
		#' @param category A link to the relevant [ProtocolParameter] that the value is set for.
		#' @param value The value of the parameter.
		#' @param unit The qualifying unit classifier, if the value is numeric.
		#' @param ontology_source_references ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
		#' @param unit_references A list of units used as a [UnitReferences] object
		#' @param protocol_parameters a named list of available [ProtocolParameter] objects
		#' @param comments Comments associated with instances of this class.
		#' @param @id identifier
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL,
			ontology_source_references = NULL,
			unit_references = NULL,
			protocol_parameters = NULL,
			comments = NULL#,
			# `@id` = character()
		){
			self$category <- category
			self$value <- value
			if(is.null(unit)) { self$unit <- unit } else {
				self$set_unit(unit)
			}
			if(is.null(ontology_source_references)) {
				self$ontology_source_references <-
					OntologySourceReferences$new()
			} else if (checkmate::test_r6(
				ontology_source_references, "OntologySourceReferences"
			)) {
				self$ontology_source_references <- ontology_source_references
			} else {stop(
				"ontology_source_references must be",
				" an OntologySourceReferences object"
			)}

			if(is.null(unit_references)) {
				self$unit_references <- UnitReferences$new(
					ontology_source_references =
						self$ontology_source_references
				)
			} else if (checkmate::test_r6(unit_references, "UnitReferences")) {
				self$unit_references <- unit_references
			} else {stop(
				"unit_references must be an UnitReferences object"
			)}
			self$protocol_parameters <- protocol_parameters
			self$comments <- comments
			# self$`@id` <- `@id`# paste0("#parameter/", gsub(" ", "_", self$value))
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
		#' Set the unit as a valid ontology term
		#' @param lst a list to be processed into a [Unit] object
		set_valid_unit = function(lst) {
			unit_id <- lst[["@id"]]
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
		#' select the protocol parameter that represents the category of this
		#' parameter value
		#' @param category the id of a protocol parameter which represents the 
		#' category of this parameter
		set_valid_category = function(category) {
			if(is.character(self$protocol_parameters)) {
				self$category <- NULL
			} else {
				self$category <- self$protocol_parameters[[category$`@id`]]
			}
		},

		#' @details
		#' generate a tabular representation of the parameter value
		#' @return a Tibble
		to_table = function(){
			comments <- NULL
			if (!test_list(self$comments, len = 0, null.ok = TRUE)) {
				comments <- self$comments %>% comment_to_table_wide()
			}

			if(checkmate::test_r6(self$category, "ProtocolParameter")) {
				parameter_value <-
					tibble::tibble("Parameter Value" = self$value) %>%
					purrr::set_names(paste0(
						"Parameter Value[", self$category$parameter_name$term,
						"]"
					))
			} else if (is.character(self$category)) {
				parameter_value <-
					tibble::tibble("Parameter Value" = self$value) %>%
					purrr::set_names(paste0(
						"Parameter Value[", self$category, "]"
					))
			} else {
				parameter_value <-
					tibble::tibble("Parameter Value" = self$value)
			}
			if (checkmate::test_r6(self$unit, "Unit")) {
				unit <- self$unit$to_table() %>% purrr::set_names(paste0(
					colnames(.), "[", self$category$parameter_name$term, "]"
				))
			} else {
				unit <- self$unit
			}

			dplyr::bind_cols(parameter_value, unit, comments)
		},
		#' @details
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		to_list = function(ld = FALSE) {
			lst <- list()
			#lst[["@id"]] <- self$`@id`
			if(checkmate::test_list(self$category, len = 0, null.ok = TRUE)){
				lst[["category"]] <- self$category
			} else {
				lst[["category"]] <- self$category$to_list()
			}
			lst[["value"]] <- self$value
			if(checkmate::test_list(self$unit, len = 0, null.ok = TRUE)){
				lst[["unit"]] <- self$unit
			} else {
				lst[["unit"]] <- self$unit$to_list()
			}
			lst[["comments"]] <- self$comments
			return(lst)
		},

		#' @details
		#'
		#' Make [Person] from list
		#'
		#' @param lst an [Person] object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				#self$`@id` <- lst[["@id"]]
				if(is.null(lst[["category"]])) {
					self$category <- NULL
				} else {
					self$set_valid_category(lst[["category"]])
				}
				self$value <- lst[["value"]]
				if(is.null(lst[["unit"]])) {
					self$unit <- NULL
				} else {
					self$set_valid_unit(lst[["unit"]])
				}
				self$comments <- lst[["comments"]]
			} else {
				# private$id <- lst[["id"]]
				self$category <- lst[["category"]]
				self$value <- lst[["value"]]
				self$unit <- lst[["unit"]]
				self$comments <- lst[["comments"]]
			}
		},

		#' @details
		#' Get the value and unit of this parameter value as a combined string
		#' @return a string
		get_value_in_units = function() {
			paste(self$value, self$units)
		},

		#' @details
		#' Pretty prints [ParameterValue] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Parameter Value"))
			green_bold_name_plain_content(
				"Category", self$category$parameter_name$term
			)
			cli::cli_text(self$value, " ", self$unit$unit$term)
			pretty_print_comments(self$comments)
		}
	)
)
