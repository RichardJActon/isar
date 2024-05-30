CharacteristicCategoryReferences <- R6::R6Class(
	"CharacteristicCategoryReferences",
	public = list(
		categories = list(),
		ontology_source_references = NULL,
		initialize = function(
			categories = list(),
			ontology_source_references = NULL
		) {
			self$categories <- categories
			self$ontology_source_references <- ontology_source_references
		},
		check_categories = function(categories) {
			if(all(purrr::map_lgl(categories, ~checkmate::check_r6(
				.x, "CharacteristicCategory"
			)))) { return(TRUE) } else {
				stop("All categories must be CharacteristicCategory objects!")
			}
		},
		add_categories = function(categories) {
			if(self$check_categories(categories)) {
				self$categories <- c(self$categories, categories)
			}
		},
		set_categories = function(categories) {
			if(self$check_categories(categories)) {
				self$categories <- categories
			}
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
		get_category_ids = function() {
			names(self$categories)
		},
		get_category_names = function() {
			purrr::map_chr(self$categories, ~.x$type$term)
		},
		from_list = function(lst, explicitly_provided = logical()) {
			lst %>%
				purrr::map(~{
					cc <- CharacteristicCategory$new(
						explicitly_provided = explicitly_provided,
						ontology_source_references =
							self$ontology_source_references
					)
					cc$from_list(.x)
					cc
				}) %>%
				purrr::set_names(., purrr::map_chr(., ~.x[["@id"]])) %>%
				self$set_categories()
		},
		print = function() {
			cli::cli_h1(cli::col_blue("Characteristic Category References"))
			purrr::walk(self$categories, ~.x$print())
		}
	)
)
