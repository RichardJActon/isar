#' R6 class for comments
#'
#' A comment allows arbitrary annotation of all commentable ISA classes
#'
#' @field name a name for the comment string
#' @field value the content of the comment string
#'
#' @importFrom checkmate check_string check_list
comment <- R6::R6Class(
	"comment",
	public = list(
		name = '',
		value = '',

		#' @details
		#' make a new comment object
		#' @param name a name for the comment string
		#' @param value the content of the comment string
		initialize = function(
			name = '',
			value = ''
		) {
			self$name <- self$set_name(name)
			self$value <- self$set_value(value)
		},
		#' @details
		#' check that name is valid
		#' @param name a name for the comment string
		check_name = function(name) {
			check <- checkmate::check_string(name)
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#' check that value is valid
		#' @param value the content of the comment string
		check_value = function(value) {
			check <- checkmate::check_string(value)
			if (isTRUE(check)) { return(TRUE) } else { stop(check) }
		},
		#' @details
		#' set a valid name
		#' @param name a name for the comment string
		set_name = function(name) {
			if(self$check_name(name)) { self$name <- name }
		},
		#' @details
		#' set a valid value
		#' @param value the content of the comment string
		set_value = function(value) {
			if(self$check_value(value)) { self$value <- value }
		},
		#' @details
		#' convert object to an R list
		#' @return a list of length 1 where name is the name and value the value
		to_list = function() {
			comment <- list(self$value)
			names(comment) <- self$name
			return(comment)
		},
		#' @details
		#' generate a comment object from an R list
		#' @param comment a list of length 1 where the name is the name and the value the value of the comment
		#' @return a list of length 1 where name is the name and value the value
		from_list = function(comment) {
			check <- checkmate::check_list(
				comment, types = "character", len = 1L, names = "named"
			)
			if(isTRUE(check)) {
				self$name <- names(comment)
				self$value <- comment[[1]]
			} else {
				stop(check)
			}
		}
	)
)
