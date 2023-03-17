#' R6 object to represent Materials
#'
#' @field name
#' @field type
#' @field characteristics
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
		name = character(),
		type = character(),
		characteristics = NULL,
		# comments

		#' @details
		#'
		#' @param name The name of the material
		#' @param type
		#' @param characteristics a list of \code{[Characteristic]} objects
		initialize = function(
			name = character(),
			type = character(),
			characteristics = NULL
		) {
			if (checkmate::qtest(name, "S[0]")) {
				self$name <- name
			} else {
				self$set_name(name)
			}
		},
		#' @details
		#' Check if the name of the material is a string
		#' @param name The name of the material
		check_name = function(name) {
			check <- checkmate::check_string(name, min.chars = 1L)
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#' set the name of the material if valid
		#' @param name The name of the material
		set_name = function(name) {
			if (self$check_name(name)) { self$name <- name }
		},
		#' @details
		#' check characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
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
		#' set characteristics if characteristics is a list of \code{[Characteristic]} objects
		#' @param characteristics a list of \code{[Characteristic]} objects
		set_characteristics = function(characteristics) {
			if (self$check_characteristics(characteristics)) {
				self$characteristics <- characteristics
			}
		},
		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		}
	),
	private = list(
		id = uuid::UUIDgenerate()
	)
)
