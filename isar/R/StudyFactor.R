#' R6 class for an experimental factor
#'
#' A [StudyFactor] corresponds to an independent variable manipulated by the
#' experimentalist with the intention to affect biological systems in a way
#' that can be measured by an assay.
#'
#'
#' @field factor_name The name of the factor
#' @field factor_type An [OntologyAnnotation] reference of the study factor type
#' @field comments Comments associated with instances of this class.
#' @field @id identifier
#' @field explicitly_provided Explicitly listed in the study as a factor (logical)
#' @field ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
#'
#' @importFrom checkmate qtest check_string
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#'
#' @export
StudyFactor <- R6::R6Class(
	"StudyFactor",
	public = list(
		factor_name = character(),
		factor_type = NULL,
		comments = NULL,
		`@id` = character(),
		origin = character(),
		ontology_source_references = NULL,
		#' @details
		#'
		#' create a new study factor
		#'
		#' @param factor_name The name of the factor
		#' @param factor_type An [OntologyAnnotation] reference of the study factor_type
		#' @param comments Comments associated with instances of this class.
		#' @param @id identifier
		#' @param origin @id of the object of origin for the factor in case it
		#' is not explicitly listed in the study as a factor (logical)
		#' @param ontology_source_references [OntologySource]s to be referenced by [OntologyAnnotation]s used in this ISA descriptor.
		#'
		initialize = function(
		factor_name = character(),
			factor_type = NULL,
			comments = NULL,
			`@id` = character(),
			origin = character(),
			ontology_source_references = NULL
		) {
			if (checkmate::qtest(factor_name, "S[0]")) {
				self$factor_name <- factor_name
			} else {
				self$set_name(factor_name)
			}
			if (is.null(factor_type)) {
				self$factor_type <- factor_type
			} else if (checkmate::check_r6(factor_type ,"OntologyAnnotation")) {
				self$factor_type <- factor_type
			} else {stop(
				"factor_type is not and ontology_annotation object or NULL!"
			)}
			self$comments <- comments
			# id format checking?
			self$`@id` <- paste0(
				"#factor/", sub("[^A-Za-z0-9]+", "_", self$factor_name)
			)
			self$origin <- origin
			self$ontology_source_references <- ontology_source_references
		},
		#' @details
		#' Check if the name of the material is a string
		#' @param factor_name The name of the material
		check_name = function(factor_name) {
			check <- checkmate::check_string(factor_name, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		#' @details
		#' set the name of the material if valid
		#' @param factor_name The name of the material
		set_name = function(factor_name) {
			if (self$check_name(factor_name)) {
				self$factor_name <- factor_name
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

		to_table = function() {
			self$factor_type$to_table() %>%
				dplyr::mutate(rowname = self$factor_name) %>%
				dplyr::relocate(rowname)
		},

		#' @details
		#' generate an R list representation translatable to JSON
		#' @return a list
		to_list = function() {
			lst = list(
				"factorType" = self$factor_type$to_list(),
				"@id" = self$`@id`,
				"factorName" = self$factor_name
				# "id" = private$id#,
				#"comments" = self$comments
			)
			return(lst)
		},

		# to_table = function() {
		#
		# },

		#' @details
		#'
		#' Make [StudyFactor] from list
		#'
		#' @param lst a list serialization of a study factor object
		#' @param json json  (default TRUE)
		#' @param recursive call to_list methods of any objects within this object (default TRUE)
		from_list = function(lst, recursive = TRUE, json = TRUE) {
			if(json) {
				self$factor_name <- lst[["factorName"]]
				self$`@id` <- lst[["@id"]]
				self$factor_type <- OntologyAnnotation$new(
					ontology_source_references =
						self$ontology_source_references
				)
				self$factor_type$from_list(lst[["factorType"]])
				# self$comments = lst[["comments"]]
			} else {
				self$factor_name <- lst[["name"]]
				private$id <- lst[["id"]]
				self$factor_type <- OntologyAnnotation$new(
					ontology_source_references =
						self$ontology_source_references
				)
				self$factor_type$from_list(lst[["factor_type"]])
				self$comments <- lst[["comments"]]
			}
		},

		#' #' @details
		#' #' Get the uuid of this object
		#' #' @return a uuid
		#' get_id = function() {
		#' 	private$id
		#' },

				#' #' @details
		#' #' set the uuid of this object
		#' #' @param id a uuid
		#' #' @param suffix a human readable suffix
		#' set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
		#' 	private$id <- generate_id(id, suffix)
		#' },

		#' @details
		#' Pretty prints [StudyFactor] objects
		print = function() {
			cli::cli_h1(cli::col_blue("Study Factor"))
			green_bold_name_plain_content("@id", self$`@id`)
			# green_bold_name_plain_content("ID", private$id)
			green_bold_name_plain_content("factor name", self$factor_name)
			green_bold_name_plain_content("factor type", self$factor_type$term)
			#pretty_print_comments(self$comments)
		}
	)# ,
	# active = list(
	# ),
	# private = list(
	# 	id = generate_id()
	# )
)
