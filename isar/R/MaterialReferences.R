#' R6 class for MaterialReferences
#'
#' @details
#'
#' @field ontology_source_references ...
#'
#' @importFrom R6 R6Class
#'
#' @export
MaterialReferences <- R6::R6Class(
	"MaterialReferences",
	public = list(
		material_references = NULL,
		initialize = function(material_references = NULL) {
			self$material_references <- material_references
		},
		#' @details
		#'
		#' Make [MaterialReferences] from list
		#'
		#' @param lst a Material object serialized to a list
		from_list = function(lst, json = TRUE) {
			material_references <- purrr::map(
				lst, ~{
					mtr <- Material$new()
					mtr$from_list(.x, json = json)
					mtr
				}
			)
			# possible problems with name collisions ?
			names(material_references) <- purrr::map_chr(
				material_references, ~.x$name
			)
			self$material_references <- material_references
		},
		#' @details
		#' check Material is a list of [Material] objects
		#' @param Material a list of [Material] objects
		check_materials = function(materials) {
			if(
				checkmate::test_list(materials, min.len = 1, names = "named") &&
				all(purrr::map_lgl(
					materials, ~checkmate::test_r6(.x, "Material")
				))
			) { return(TRUE) } else {
				stop("All materials must be Material objects")
			}
		},
		#' @details
		#' Add ontology sources to the
		#' @return a vector of ontology source names
		add_materials = function(materials) {
			if(self$check_materials(materials)) {
				self$material_references <- c(
					self$material_references, materials
				)
			}
		},
		#' @details
		#' Get the names of the Ontology Sources Used in this Investigation
		#' @return a vector of ontology source names
		get_material_reference_names = function() {
			names(self$material_references)
		}

	)
)

