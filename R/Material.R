#' R6 object to represent Materials
#'
#' @field name name of the material
#' @field type the type of the material
#' @field characteristics the characteristics of the material in the form of a [Characteristics] object
#' @field characteristic_categories an [CharacteristicCategoryReferences] object
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
#' @field unit_references A list of units used as a [UnitReferences] object.
#' @field comments comments
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate qtest check_string
#' @importFrom purrr map_lgl
#' @importFrom uuid UUIDgenerate
#'
#' @export
Material <- R6::R6Class(
	"Material",
	public = list(
		# material type as distinct from type?
		# desription?
		name = character(),
		type = character(),
		characteristics = NULL,
		characteristic_categories = NULL,
		ontology_source_references = NULL,
		unit_references = NULL,
		comments = NULL,
		`@id` =  character(),
		#' @details
		#' Create a new [Material] object
		#' @param name The name of the material
		#' @param type the type of the material
		#' @param characteristics a list of [Characteristic] objects
		#' @param characteristic_categories an [CharacteristicCategoryReferences] object
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
		#' @param unit_references A list of units used as a [UnitReferences] object.
		#' @param comments comments
		#' @param @id identifier
		initialize = function(
			name = character(),
			type = character(),
			characteristics = NULL,
			characteristic_categories = NULL,
			ontology_source_references = NULL,
			unit_references = NULL,
			comments = NULL,
			`@id` =  character()
		) {
			if (checkmate::qtest(name, "S[0]")) { self$name <- name } else {
				self$set_name(name)
			}
			if (checkmate::qtest(type, "S[0]")) { self$type <- type } else {
				self$set_type(type)
			}
			if(is.null(characteristics)) {
				self$characteristics <- characteristics
			} else {
				self$check_characteristics(characteristics)
			}
			self$comments <- comments
			self$characteristic_categories <- characteristic_categories
			self$ontology_source_references <- ontology_source_references
			self$unit_references <- unit_references
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
		#' Check the the type has a non-zero length
		#' @param type of the material
		check_type = function(type) {
			check <- checkmate::check_string(type, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' Set the type of the [Material]
		#' @param type of the material
		set_type = function(type) {
			if (self$check_type(type)) { self$type <- type }
		},
		#' @details
		#' check characteristics is a list of [Characteristic] objects
		#' @param characteristics a list of [Characteristic] objects
		check_characteristics = function(characteristics) {
			if(
				checkmate::test_list(characteristics, min.len = 1) &&
				all(purrr::map_lgl(
					characteristics, ~checkmate::test_r6(.x, "Characteristic")
				))
			) { return(TRUE) } else {
				stop("All characteristics must be Characteristic objects")
			}
		},
		#' @details
		#' set characteristics if characteristics is a list of [Characteristic] objects
		#' @param characteristics a list of [Characteristic] objects
		set_characteristics = function(characteristics) {
			if (self$check_characteristics(characteristics)) {
				self$characteristics <- characteristics
			}
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
		#' generate a tabular representation of a material object
		#' @return a Tibble
		to_table = function() {
			# comments!
			name_type <- tibble::tibble_row(
				ifelse(test_empty(self$name, null.ok = TRUE), "", self$name)
			) %>%
				purrr::set_names(self$type)
			characteristics <- NULL
			if (!test_list(self$characteristics, len = 0, null.ok = TRUE)) {
				characteristics <- self$characteristics %>%
					purrr::map(~.x$to_table()) %>%
					# purrr::list_cbind(name_repair = "minimal")
					purrr::list_rbind()
			}
			dplyr::bind_cols(name_type, characteristics)
		},
		#' @details
		#' An R list representation of a [Material] object
		#' @param ld linked data (default FALSE)
		to_list = function(ld = FALSE) {
			lst <- list()
			lst[["@id"]] <- self$`@id`
			lst[["name"]] <- private$raw_name
			lst[["type"]] <- self$type
			lst[["characteristics"]] <- self$characteristics %>%
				purrr::map(~.x$to_list())
				#"comments" = self$comments
			return(lst)
		},
		#' @details
		#' Make [Material] object from list
		#' @param lst an Material object serialized to a list
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			# self$name <- lst[["name"]]
			private$raw_name <- lst[["name"]]
			self$name <- sub(".*?-(.*)", "\\1", private$raw_name)

			#private$id <- lst[["id"]]
			self$`@id` <- lst[["@id"]]
			self$type <- lst[["type"]]
			self$characteristics <- purrr::map(lst[["characteristics"]], ~{
				chr <- Characteristic$new(
					ontology_source_references =
						self$ontology_source_references,
					category_references = self$characteristic_categories,
					unit_references = self$unit_references
				)
				chr$from_list(.x, json = json, recursive = recursive)
				chr
			})
			self$comments <- lst[["comments"]]
		},

		# #' @details
		# #' Get the uuid of this object
		# #' @return a uuid
		# get_id = function() {
		# 	private$id
		# },

		#' @details
		#' set the uuid of this object
		#' @param id a uuid
		#' @param suffix a human readable suffix
		set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
			private$id <- generate_id(id, suffix)
		},
		#' @details
		#' Pretty prints [Materials] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Material"))
			green_bold_name_plain_content("Name", self$name)
			green_bold_name_plain_content("Type", self$type)
			# green_bold_name_plain_content("ID", self$get_id())
			green_bold_name_plain_content("@id", self$`@id`)
			cli::cli_h2(cli::col_cyan("Characteristics"))
			purrr::walk(
				self$characteristics, ~{
					cli::cli_text(paste0("(", .x$category$type$term, ")"))
					green_bold_name_plain_content(
						.x$value$term_source$name, .x$value$term
					)
				}
			)

			#green_bold_name_plain_content("unit", self$unit)
			pretty_print_comments(self$comments)
		}
	),
	private = list(
		# id = generate_id()
		raw_name = character()
	)
)
