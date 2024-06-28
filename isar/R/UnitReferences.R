#' R6 class for UnitReferences
#'
#' @field unit_references ...
#'
#' @importFrom R6 R6Class
#'
#' @export
UnitReferences <- R6::R6Class(
	"UnitReferences",
	public = list(
		unit_references = NULL,
		ontology_source_references = NULL,
		initialize = function(
			unit_references = NULL,
			ontology_source_references = NULL
		) {
			self$unit_references <- unit_references
			self$ontology_source_references <- ontology_source_references
		},
		check_unit_references = function(units) {
			if (
				checkmate::test_list(units, names = "unique", min.len = 1) &&
				#checkmate::test_list(units, min.len = 1) &&
				all(purrr::map_lgl(units, ~checkmate::test_r6(.x, "Unit")))
			) { return(TRUE) } else{
				stop("Unit references must be a uniquely named list of Unit objects!")
			}
		},
		set_unit_references = function(units) {
			if(self$check_unit_references(units)) {
				self$unit_references <- units
			}
		},
		add_unit_references = function(units) {
			comb <- c(self$unit_references, units)
			if(self$check_unit_references(comb)) {
				self$unit_references <- comb
			}
		},
		get_unit_ids = function(){
			names(self$unit_references)
		},
		to_list = function() {
			purrr::map(self$unit_references, ~.x$to_list()) %>%
				purrr::set_names(NULL)
		},
		from_list = function(lst, source = NA, add = FALSE) {
			# browser()
			ur <- lst %>%
				purrr::set_names(purrr::map_chr(., ~.x$`@id`)) %>%
				purrr::map(~{
					u <- Unit$new(
						ontology_source_references =
							self$ontology_source_references#,
						# unit_references = self$unit_references,
						source = source
					)
					u$from_list(.x)
					u
				})
			if(add){
				self$add_unit_references(ur)
			} else {
				self$set_unit_references(ur)
			}
		}
	)
)
