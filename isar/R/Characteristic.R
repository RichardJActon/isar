#' Characteristic
#'
#' A Characteristic acts as a qualifying property to a material object.
#'
#' @field category The classifier of the type of characteristic being described.
#' @field value The value of this instance of a characteristic as relevant to the attached material.
#' @field unit If applicable, a unit qualifier for the value (if the value is numeric).
#' @field category_references an [CharacteristicCategoryReferences] object
#' @field comments comments
#' @field @id identifier
#' @field ontology_source_references an [OntologySourceReferences] object
#' @field unit_references an [UnitReferences] object
#'
#' @export
Characteristic <- R6::R6Class(
	"Characteristic",
	public = list(
		category = NULL,
		value = NULL,
		unit = NULL,
		category_references = NULL,
		comments = NULL,
		`@id` =  character(),
		ontology_source_references = NULL,
		unit_references = NULL,
		#' @details
		#' Create a new [Characteristics] object
		#' @param category The classifier of the type of characteristic being described.
		#' @param value The value of this instance of a characteristic as relevant to the attached material.
		#' @param unit If applicable, a unit qualifier for the value (if the value is numeric).
		#' @param category_references an [CharacteristicCategoryReferences] object
		#' @param comments comments
		#' @param @id identifier
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param unit_references an [UnitReferences] object
		initialize = function(
			category = NULL,
			value = NULL,
			unit = NULL,
			category_references = NULL,
			comments = NULL,
			`@id` = character(),
			ontology_source_references = NULL,
			unit_references = NULL
		) {
			if(is.null(category)) {
				self$category <- category
			} else {
				self$set_category(category)
			}
			self$value <- value
			self$unit <- unit
			if(is.null(category_references)) {
				self$category_references <-
					CharacteristicCategoryReferences$new()
			} else {
				self$category_references <- category_references
			}
			self$comments <- comments
			self$`@id` <- `@id`
			if(is.null(ontology_source_references)) {
				self$ontology_source_references <-
					OntologySourceReferences$new()
			} else {
				self$ontology_source_references <- ontology_source_references
			}
			if(is.null(unit_references)) {
				self$unit_references <- UnitReferences$new(
					ontology_source_references = self$ontology_source_references
				)
			} else {
				self$unit_references <- unit_references
			}
		},
		#' @details
		#' Check that category is an [CharacteristicCategory] object
		#' @param category an [CharacteristicCategory] object
		check_category = function(category) {
			check <- checkmate::check_r6(category, "CharacteristicCategory")
			error_with_check_message_on_failure(
				check, nextline = "Class: CharacteristicCategory"
			)
		},
		#' @details
		#' Set category if input is valid
		#' @param category an [CharacteristicCategory] object
		set_category = function(category) {
			if(self$check_category(category)) { self$category <- category }
		},
		#' @details
		#' check that value is numeric
		#' @param value a value in the specified units
		check_value = function(value) { ###!!! condition on unit being not null for numeric, and null for r6 ont anno !!###
			if (is.numeric(value)) { return(TRUE) }
		},
		#' @details
		#' set the value if input is valid
		#' @param value a value in the specified units
		set_value = function(value) {
			if(self$check_value(value)) { self$value <- value }
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

		#set_valid_category_and_value = function(category, value) {
		#' @details
		#' Set a category and check that it is valid
		#' @param category a category
		set_valid_category = function(category) {
			#if (self$check_category(category)) {
			#browser()
				if (
					category$`@id` %in%
					self$category_references$get_category_ids()
				) {
					self$category <-
						self$category_references$categories[[category$`@id`]]
				} else if(!is.null(category$`@id`)) {
					warning("Unlisted Characteristic Category!")
					self$category_references$categories[[
						category$`@id`
					]] <- CharacteristicCategory$new(
						`@id` = category$`@id`,
						ontology_source_references =
							self$ontology_source_references,
						explicitly_provided = FALSE,
						source = category$`@id`
					)
					self$category <- self$category_references$categories[[
						category$`@id`
					]]
				} else {
					self$category_references$categories[[
						"UnknownCharacteristic"
					]] <- CharacteristicCategory$new(
							`@id` = "Unspecified",
							ontology_source_references =
								self$ontology_source_references,
							explicitly_provided = FALSE,
							source = category$`@id`
						)
					self$category <- self$category_references$categories[[
						"UnknownCharacteristic"
					]]
				}
			#}
		},

		#' @details
		#' Set the unit as a valid ontology term
		#' @param term the term of the unit
		#' @param term_accession the accession of the ontology term of the unit
		#' @param term_source the name of the source of the ontology term
		#set_valid_unit = function(unit_id, term, term_accession, term_source) {
		set_valid_unit = function(lst) {
			# browser()
			# if (!is.null(term)) {
			# 	if (term %in% OM$terms_list || term %in% names(OM$terms_list)) {
			# 		if(
			# 			!"OM" %in%
			# 			self$ontology_source_references$get_ontology_source_names()
			# 		) {
			# 			self$ontology_source_references$add_ontology_sources(
			# 				list("OM" = OM)
			# 			)
			# 		}
			# 	}
			# }

			# if (is.null(self$unit_references)) {
			# 	self$unit_references <- UnitReferences$new()
			# }
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
		#' An R list representation of a [Characteristic] object
		#' @param ld linked data (default FALSE)
		#' @param recursive call to_list methods of any objects within this
		#' object (default FALSE)
		to_list = function(ld = FALSE, recursive = TRUE) {
			lst <- list()
			if(recursive) {
				# browser()
				# lst[["@id"]] <- self$`@id`
				lst[["category"]][["@id"]] <- self$category$`@id`
				if(!is.null(self$unit)) {
					lst[["unit"]] <- self$unit$to_list(recursive = FALSE)
				}
				if(checkmate::test_r6(self$value, "OntologyAnnotation")) {
					lst[["value"]] <- self$value$to_list()
				} else {
					lst[["value"]] <- self$value
				}
				lst[["comments"]] <- self$comments

			} else {
				lst <- list(
					"@id" = self$`@id`,
					"category" = self$category$term,
					"value" = self$value$term,
					"unit" = self$unit$unit$term,
					"comments" = self$comments
				)
			}
			return(lst)
		},
		#' @details
		#' Make [Characteristic] object from list
		#' @param lst an Characteristic object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			self$`@id` <- lst[["category"]][["@id"]]
			self$set_valid_category(lst[["category"]])
			if (is.null(lst[["unit"]])) {
				self$value <- OntologyAnnotation$new(
					ontology_source_references =
						self$ontology_source_references
				)
				self$value$from_list(
					lst[["value"]], recursive = recursive, json = json
				)
			} else {
				self$set_valid_unit(
					lst[["unit"]]
					# lst[["unit"]][["@id"]],
					# lst[["unit"]][["annotationValue"]],
					# lst[["unit"]][["termAccession"]],
					# lst[["unit"]][["termSource"]]
				)
				# self$unit <- Unit$new(
				# 	ontology_source_references =
				# 		self$ontology_source_references,
				# 	unit_references = self$unit_references
				# )
				# self$unit$from_list(lst[["unit"]])
				#
				# set_valid_unit()
				self$set_value(lst[["value"]])
			}
			self$set_comments(lst[["comments"]])
			#
			# if(json) {
			# 	self$set_id()
			# 	if(is.null(lst[["@id"]])) {
			# 		self$`@id` <- lst[["category"]][["@id"]]
			# 		ont_anno <- lst[["value"]]
			# 	} else {
			# 		self$`@id` <- lst[["@id"]]
			# 		ont_anno <- lst[["characteristicType"]]
			# 	}
			# 	# self$category <- sub(
			# 	# 	"#characteristic_category/", "", self$`@id`, fixed = TRUE
			# 	# )
			# 	self$set_valid_category(lst[["category"]])
			# 	if (is.null(lst[["unit"]])) {
			# 		self$value <- OntologyAnnotation$new(
			# 			ontology_source_references =
			# 				self$ontology_source_references
			# 		)
			# 		self$value$from_list(
			# 			ont_anno, recursive = recursive, json = json
			# 		)
			# 	} else {
			# 		self$value <- lst[["value"]]
			# 	}
			# 	self$set_comments(lst[["comments"]])
			# }
			#
			#
			# else {
			# 	private$id <- lst[["id"]]
			# 	if(recursive) {
			# 		self$category <- OntologyAnnotation$new()
			# 		self$category$from_list(lst[["category"]])
			# 	} else {
			# 		if(checkmate::test_r6(
			# 			lst[["category"]], "OntologyAnnotation"
			# 		)) {
			# 			stop(
			# 				"not a list contains raw OntologyAnnotation object"
			# 			)
			# 		} else if(is.null(lst[["category"]])) {
			# 			self$category <- NULL
			# 		} else {
			# 			self$category$term <- lst[["category"]]
			# 		}
			# 	}
			# 	self$value <- lst[["value"]]
			# 	if(recursive) {
			# 		self$unit <- Unit$new()
			# 		self$unit$from_list(lst[["unit"]])
			# 	} else {
			# 		if(checkmate::test_r6(
			# 			lst[["unit"]], "Unit"
			# 		)) {
			# 			stop("not a list contains raw Unit object")
			# 		} else if(is.null(lst[["unit"]])) {
			# 			self$unit <- NULL
			# 		} else {
			# 			self$unit$unit$term <- lst[["unit"]]
			# 		}
			# 	}
			# 	self$comments <- lst[["comments"]]
			# }
		},

		#' #' @details
		#' #' set the uuid of this object
		#' #' @param id a uuid
		#' #' @param suffix a human readable suffix
		#' set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
		#' 	private$id <- generate_id(id, suffix)
		#' },
		#' @details
		#' Pretty Prints [Characteristic] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Characteristic"))
			green_bold_name_plain_content("category", self$category[["@id"]])
			# green_bold_name_plain_content("ID", self$get_id())
			if(is.null(self$unit)) {
				green_bold_name_plain_content("value", self$value$term)
			} else {
				green_bold_name_plain_content("unit", self$unit$term)
			}
			pretty_print_comments(self$comments)
		}
	)
)

