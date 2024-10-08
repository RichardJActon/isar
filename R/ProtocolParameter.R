#' A parameter used by a protocol.
#'
#' @field parameter_name A parameter name as an ontology term
#' @field ontology_source_references an [OntologySourceReferences] object
#' @field comments Comments associated with instances of this class.
#' @field @id identifier
#'
#' @importFrom R6 R6Class
#' @importFrom checkmate check_r6
#'
#' @export
ProtocolParameter <- R6::R6Class(
	"ProtocolParameter",
	public = list(
		parameter_name = NULL,
		ontology_source_references = NULL,
		comments = NULL,
		`@id` = character(),
		#' @details
		#' Create a new [ProtocolParameter] object
		#' @param parameter_name A parameter name as an ontology term
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param comments Comments associated with instances of this class.
		#' @param @id identifier
		initialize = function(
			parameter_name = NULL,
			ontology_source_references = NULL,
			comments = NULL,
			`@id` = character()
		) {
			if(is.null(parameter_name)) {
				self$parameter_name <- parameter_name
			} else {
				self$set_parameter_name(parameter_name)
			}
			self$set_ontology_source_references(
				ontology_source_references, null.action = "create"
			)
			self$comments <- comments
			self$`@id` <- `@id`
		},
		#' @details
		#' 
		#' @param ontology_source_references an [OntologySourceReferences] object
		#' @param null.action how to handle NULLs:
		#' - "error" thow an error 
		#' - "passthrough" set to NULL
		#' - "create" set to an empty  [OntologySourceReferences] object
		set_ontology_source_references = function(ontology_source_references, null.action) {
			set_ontology_source_references(self, ontology_source_references, null.action)
		},
		
		#' @details
		#' 
		#' returns TRUE if ontology_source_references is an [OntologySourceReferences]
		#' object and throws an error if it is not
		#'
		#' @param ontology_source_references something you want to check is an
		#' [OntologySourceReferences] object.
		check_ontology_source_references = function(ontology_source_references) {
			check_ontology_source_references(ontology_source_references)
		},
		#' @details
		#' Check that parameter_name is an [OntologyAnnotation] object
		#' @param parameter_name an [OntologyAnnotation] object
		check_parameter_name = function(parameter_name) {
			check <- checkmate::check_r6(parameter_name, "OntologyAnnotation")
			error_with_check_message_on_failure(
				check, nextline = "Class: OntologyAnnotation"
			)
		},
		#' @details
		#' Set parameter_name if input is valid
		#' @param parameter_name an [OntologyAnnotation] object
		set_parameter_name = function(parameter_name) {
			if(self$check_parameter_name(parameter_name)) {
				if(
					!parameter_name$term_source$name %in% 
					self$ontology_source_references$get_ontology_source_names()
				) { 
					parameter_name$term_source %>%
						self$ontology_source_references$add_ontology_source()
					warning(
						"ontology annotation source was not present in the",
						" OntologySourceReferences!\n",
						"Attempting to add it ..."
					)
				}
				self$parameter_name <- parameter_name
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
		#' generate an R list representation translatable to JSON
		#' @param ld logical json-ld
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		#' @examples
		#' Person$new()
		to_list = function(ld = FALSE, recursive = TRUE) {
			lst <- list(
				#"id" = private$id,
				"parameterName" = switch(
					as.character(recursive),
					"TRUE" = self$parameter_name$to_list(),
					"FALSE" = self$parameter_name$term
				),
				"comments" = self$comments,
				"@id" = self$`@id`
			)
			return(lst)
		},

		#' @details
		#'
		#' Make [OntologyAnnotation] from list
		#'
		#' @param lst an [OntologyAnnotation] object serialized to a list
		#' @param recursive use the `from_list()` method on list items that are also isar objects (default = TRUE)
		from_list = function(lst, recursive = TRUE) {
			# private$id <- lst[["id"]]
			self$`@id` <- lst[["@id"]]
			if(recursive) {
				self$parameter_name <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
				self$parameter_name$from_list(lst[["parameterName"]])
			} else {
				if(checkmate::test_r6(
					lst[["parameter_name"]], "OntologyAnnotation"
				)) {
					stop("not a list contains raw OntologyAnnotation object")
				} else if(is.null(lst[["parameterName"]])) {
					self$parameter_name <- NULL
				} else {
					self$parameter_name$term <- lst[["parameterName"]]
				}
			}
			self$comments <- lst[["comments"]]
		},

		#' @details
		#' Pretty Prints [ProtocolParameter] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Protocol Parameter"))
			green_bold_name_plain_content("Name", self$parameter_name$term)
			green_bold_name_plain_content(
				"Term Accession", self$parameter_name$term_accession
			)
			green_bold_name_plain_content(
				"Term Source", self$parameter_name$term_source$name
			)
			green_bold_name_plain_content("@id", self$`@id`)
			# green_bold_name_plain_content("ID", private$id)
			pretty_print_comments(self$comments)
		}
	)
)

