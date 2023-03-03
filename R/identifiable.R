#' R6 superclass for identifiable objects
#'
#' option to prefix with custom name.
#'
#' @field id of the id object NULL
#'
#' @importFrom uuid UUIDgenerate
identifiable <- R6::R6Class(
	"identifiable",
	public = list(
		id = NULL,

		#' @details
		#'
		#' make a new identifiable object
		#'
		#' @param id default NULL
		initialize = function(id = NULL) {
			self$id <- id
		},

		#' @details
		#'
		#' set self id to a uuid
		#'
		#' @examples
		#' \dontrun{
		#' super$set_id()
		#' }
		#'
		#' @return null
		set_id = function() {
			self$id <- uuid::UUIDgenerate()
		}
	)
)
