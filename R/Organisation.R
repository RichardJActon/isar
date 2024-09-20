#' Organisation
#'
#' @field comments comments associated with instances of this class.
#'
#' @importFrom R6 R6Class
#'
#' @export
Organisation <- R6::R6Class(
	"Organisation",
	public = list(
		comments = NULL,
		#' @details
		#' make a new Organisation object
		#' @param comments Comments associated with instances of this class.
		initialize = function(
			comments = NULL
		) {
			self$set_comments(comments)
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
		#' @examples
		#' Organisation$new()
		to_list = function(ld = FALSE) {
			person = list(
				# "id" = private$id,

				"comments" = self$comments
			)
			return(person)
		},

		#' @details
		#'
		#' Make [Organisation] from list
		#'
		#' @param lst an [Organisation] object serialized to a list
		from_list = function(lst) {
			self$comments <- lst[["comments"]]
		}
	)
)
