#' An R6 object for representing ParameterValue
#'
#' A ParameterValue represents the instance value of a [ProtocolParameter],
#' used in a Process.
#' You cannot create a [ParameterValue] object without providing a [Protocol]
#' object.
#' A [Process] entails the execution of a [Protocol].
#' [Protocol]s have parameters the values of which ([ParameterValue]s) can be
#' specified in a [Process] executing that [Protocol].
#' The 'categories' of [ParameterValue]s available are exposed by the
#' [Protocol].
#' Thus in order to know the available categories of [ParameterValue]s the
#' [Protocol] being executed must be specified.
#' A 'placeholder' [Protocol] is not automatically generated as unlike
#' [OntologySources] in and [OntologySourceReference] [ParameterValue]
#' categories are not intended to be exposed to all [Process]es only those
#' which execute the [Protocol]s that expose them.
#'
#' @field category A link to the relevant [ProtocolParameter] that the value is
#' set for.
#' @field value The value of the parameter.
#' @field unit The qualifying unit classifier, if the value is numeric.
#' @field ontology_source_references ontology_source_references
#' [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA
#' descriptor.
#' @field unit_references A list of units used as a [UnitReferences] object
#' @field protocol The [Protocol] object that exposes the category of this
#' parameter.
#' @field protocol_references The [Protocol References] object that exposes the category of this parameter.
#' @field comments Comments associated with instances of this class.
# #' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
ParameterValue <- R6::R6Class(
#
# Don't think it makes sense to have a default protocol which would contain all categories without one, better to fail and require manual? - let's see?
	"ParameterValue",
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		protocol = NULL,
		protocol_references = NULL,
		comments = NULL,
		#`@id` = character(),
		#' @details
		#' New [ParameterValue] object
		#' @param category A link to the relevant [ProtocolParameter] that the value is set for.
		#' @param value The value of the parameter.
		#' @param unit The qualifying unit classifier, if the value is numeric.
		#' @param ontology_source_references ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
		#' @param unit_references A list of units used as a [UnitReferences] object
		#' @param protocol The [Protocol] object that exposes the category of this parameter.
		#' @param protocol_references The [Protocol References] object that exposes the category of this parameter.
		#' @param comments Comments associated with instances of this class.
		# #' @param @id identifier
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL,
			ontology_source_references = NULL,
			unit_references = NULL,
			protocol = NULL,
			protocol_references = NULL,
			comments = NULL#,
			# `@id` = character()
		){
			self$category <- category
			self$value <- value
			self$set_ontology_source_references(
				ontology_source_references, null.action = "create"
			)
			self$set_unit_references(unit_references, null.action = "create")
			if(is.null(unit)) { self$unit <- unit } else {
				self$set_unit(unit)
			}
			self$set_protocol_references(
				protocol_references, null.action = "passthrough"
			)
			self$set_protocol(protocol, null.action = "passthrough")
			self$comments <- comments
			# self$`@id` <- `@id`# paste0("#parameter/", gsub(" ", "_", self$value))
		},
		#' @details
		#'
		#' specify the ontology source references for the [Protocol]
		#'
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param null.action how to handle NULLs:
		#' - "error" throw an error
		#' - "passthrough" set to NULL
		#' - "create" set to an empty  [OntologySourceReferences] object
		set_ontology_source_references = function(ontology_source_references, null.action) {
			set_ontology_source_references(self, ontology_source_references, null.action)
		},

		#' @details
		#'
		#' specify the protocol references for the [Protocol]
		#'
		#' @param protocol_references an [ProtocolReferences] object
		#' @param null.action how to handle NULLs:
		#' - "error" throw an error
		#' - "passthrough" set to NULL
		#' - "create" set to an empty  [ProtocolReferences] object
		set_protocol_references = function(protocol_references, null.action) {
			set_protocol_references(self, protocol_references, null.action)
		},

		#' @details
		#'
		#' specify the unit references for the [Protocol]
		#'
		#' @param unit_references an [UnitReferences] object
		#' @param null.action how to handle NULLs:
		#' - "error" throw an error
		#' - "passthrough" set to NULL
		#' - "create" set to an empty  [UnitReferences] object
		set_unit_references = function(unit_references, null.action) {
			set_unit_references(self, unit_references, null.action)
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
		#' set the protocol object
		#' @param protocol a protocol object
		#' @param null.action how to handle NULLs:
		#' - "error" throw an error
		#' - "passthrough" set to NULL
		#' - "create" set to an empty  [ProtocolReferences] object
		set_protocol = function(protocol, null.action = "error") {
			if(is.null(protocol)) {
				switch(null.action,
					"error" = {
						stop("protocol must be a Protocol object!")
					},
					"passthrough" = { self$protocol <- NULL },
					"create" = {
						pid <- "#protocol/Unknown"
						self$set_protocol_references(
							self$protocol_references, null.action = "create"
						)
						Protocol$new(
							ontology_source_references =
								self$ontology_source_references,
							protocol_references = self$protocol_references,
							# origin = self$`@id`,
							origin = paste("generated protocol for parameter value", pid),
							name = "Unknown Protocol",
							`@id` = pid
						) %>%
							list() %>%
							purrr::set_names(pid) %>%
							self$protocol_references$add_protocols()
						self$protocol <-
							self$protocol_references$protocols[[pid]]
					}
				)
			} else if(
				checkmate::test_r6(protocol, "Protocol")
			) {
				if(
					!protocol$`@id` %in%
						self$protocol_references$get_protocol_ids()
				) {
					protocol %>% list() %>% purrr::set_names(protocol$`@id`) %>%
					self$protocol_references$add_protocols()
					warning(
						"Protocol Not Found in PrococolReferences!\n",
						"Attempting to add it to the reference..."
					)
				}
				# print(paste("protocol @id:", protocol$`@id`))
				self$protocol <- self$protocol_references$protocols[[
					protocol$`@id`
				]]
				# print(paste("protocol @id after ref asign:", protocol$`@id`))
				# print(
				# 	paste("protocol reference with the protocol @id:",
				# 	self$protocol_references$protocols[[
				# 		protocol$`@id`
				# 	]]$`@id`
				# ))
				# print(paste("self protocol @id:",self$protocol$`@id`))
				# print(paste(
				# 	"self protocol reference ids:\n",
				# 	paste(self$protocol_references$get_protocol_ids(),collapse = "\n")
				# ))
			} else {
				pid <- protocol$`@id`
				Protocol$new(
					ontology_source_references =
						self$ontology_source_references,
					protocol_references = self$protocol_references,
					origin = paste("generated protocol for parameter value", pid),
					name = "Unknown Protocol",
					`@id` = pid
				) %>%
					list() %>%
					purrr::set_names(pid) %>%
					self$protocol_references$add_protocols()
				self$protocol <-
					self$protocol_references$protocols[[pid]]
			}
		},
		#' @details
		#' select the protocol parameter that represents the category of this
		#' parameter value
		#' @param category the id of a protocol parameter which represents the
		#' category of this parameter
		set_valid_category = function(category) {

			# self$set_protocol(self$protocol, null.action = "create")
			# print(paste("self protocol @id from set category:", self$protocol$`@id`))
			# print(self$protocol_references$get_protocol_ids())
			# print(paste(
			# 	"self protocol reference ids from set category:\n",
			# 	paste(self$protocol_references$get_protocol_ids(),collapse = "\n")
			# ))

			if (category$`@id` %in% names(self$protocol$parameters)) {
				self$category <- self$protocol$parameters[[category$`@id`]]
			} else {
				ProtocolParameter$new(
					ontology_source_references =
						self$ontology_source_references,
					`@id` = category$`@id`,
					parameter_name = OntologyAnnotation$new(
						ontology_source_references = self$ontology_source_references
					)
				) %>%
					list() %>%
					purrr::set_names(category$`@id`) %>%
					self$protocol$add_parameters()
				self$category <- self$protocol$parameters[[category$`@id`]]
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
			if(checkmate::test_list(self$category, len = 0, null.ok = TRUE)) {
				lst[["category"]] <- self$category
			} else {
				# lst[["category"]] <- self$category$to_list()
				lst[["category"]][["@id"]] <- self$category$`@id`
			}
			lst[["value"]] <- self$value
			if(checkmate::test_list(self$unit, len = 0, null.ok = TRUE)) {
				lst[["unit"]] <- self$unit
			} else {
				# lst[["unit"]] <- self$unit$to_list()
				lst[["unit"]]["@id"] <- self$unit$`@id`
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
				self$set_protocol(self$protocol, null.action = "create")
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
