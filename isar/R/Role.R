#' R6
#'
#' @field comments comments associated with instances of this class.
#'
#' @importFrom uuid UUIDgenerate
#'
#' @export
Role <- R6::R6Class(
	"Role",
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
		#' Role$new()
		to_list = function(ld = FALSE) {
			person = list(
				"id" = private$id,

				"comments" = self$comments
			)
			return(person)
		},

		#' @details
		#'
		#' Make \code{[Role]} from list
		#'
		#' @param lst an \code{[Role]} object serialized to a list
		from_list = function(lst) {
			private$id <- lst[["id"]]

			self$comments <- lst[["comments"]]
		},
		#' @details
		#' Get the uuid of this object
		#' @return a uuid
		get_id = function() {
			private$id
		},
		#' @details
		#' set the uuid of this object
		#' @param id a uuid
		#' @param suffix a human readable suffix
		set_id = function(id = uuid::UUIDgenerate(), suffix = character()) {
			private$id <- generate_id(id, suffix)
		}
	),
	private = list(
		id = generate_id()
	)
)
