CharacteristicCategory <- R6::R6Class(
	"CharacteristicCategory",
	public = list(
		`@id` = character(),
		type = NULL,
		explicitly_provided = logical(),
		ontology_source_references = NULL,
		initialize = function(
			`@id` = character(),
			type = NULL,
			explicitly_provided = logical(),
			ontology_source_references = NULL
		) {
			if(test_empty(`@id`, mode = "character")) {
				self$`@id` <- `@id`
			} else {
				self$set_id(`@id`)
			}
			self$set_type(type)
			self$explicitly_provided <- explicitly_provided # checks!
			self$ontology_source_references <- ontology_source_references
		},
		check_id = function(id) {
			check <- checkmate::check_string(id, min.chars = 1L)
			error_with_check_message_on_failure(check)
		},
		set_id = function(id) {
			if(self$check_id(id)) { self$`@id` <- id }
		},
		check_type = function(type) {
			check <- checkmate::check_r6(
				type, "CharacteristicCategory", null.ok = TRUE
			)
			error_with_check_message_on_failure(check)
		},
		set_type = function(type) {
			if(self$check_type(type)) { self$type <- type }
		},
		from_list = function(lst) {
			self$set_id(lst[["@id"]])
			self$type <- {
				oa <- OntologyAnnotation$new(
					ontology_source_references = self$ontology_source_references
				)
				oa$from_list(lst[["characteristicType"]])
				oa
			}
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Characteristic Category"))
			green_bold_name_plain_content("@id", self[["@id"]])
			green_bold_name_plain_content("Type", self$type$term)
			green_bold_name_plain_content(
				"Explicitly Provided", self$explicitly_provided
			)

			# cli::cli_h2(cli::col_green("Type"))
			# cli::cli_ul(self$type$term)
			# self$type$print()
		}
	)
)
