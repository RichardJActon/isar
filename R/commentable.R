#' R6 superclass for making classes commentable
#'
#' @field comments
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map_lgl imap map_chr
#' @importFrom checkmate check_r6
commentable <- R6::R6Class(
	"commentable",
	public = list(
		comments = NULL,
		#' @details
		#' create a new \code{[commentable]} object
		#' @param comments a list of \code{[comment]} objects
		initialize = function(
			comments = list(comment$new())
		) {
			self$set_comments(comments)
		},
		#' @details
		#' check comments is a list of \code{[comment]} objects
		#' @param comments a list of \code{[comment]} objects
		check_comments = function(comments) {
			if(all(
				purrr::map_lgl(comments, ~checkmate::check_r6(.x, "comment"))
			)) { return(TRUE) } else {
				stop("All comments must be comment objects")
			}
		},
		#' @details
		#' set comments if comments is a list of \code{[comment]} objects
		#' @param comments a list of \code{[comment]} objects
		set_comments = function(comments) {
			if (self$check_comments(comments)) { self$commments <- comments }
		},
		#' @details
		#' get comments as a list of \code{[comment]} objects
		#' @return comments a list of \code{[comment]} objects
		get_comments = function() {
			self$comments
		},
		#' @details
		#' get a specific comment by name as a \code{[comment]} object
		#' @param name name of the comment to retrieve
		#' @return selected comment a \code{[comment]} object
		get_comment = function(name) {
			#self$comments$to_list()[[name]]
			# efficiency?
			comment_names <- self$get_comment_names()
			if(name %in% comment_names) {
				return(self$comments[which(comment_names == name)])
			} else {stop(
				"No comment by this name!, use get_comment_names() method to see valid names"
			)}
		},
		#' @details
		#' get the names of all comments
		#' @return a character vector of comment names
		get_comment_names = function() {
			# efficiency?
			purrr::map_chr(self$comments, ~.x$name())
		},
		#' @details
		#' get the values of all comments
		#' @return a character vector of comment values
		get_comment_values = function() {
			# efficiency?
			purrr::map_chr(self$comments, ~.x$value())
		},
		#' @details
		#' append a new \code{[comment]} object to the existing list of comments
		#' @param comment a \code{[comment]} object to append to the existing \code{[commentable]} object
		#' @return a character vector of comment values
		add_comment = function(comment) {
			check <- checkmate::check_r6(comment, "comment")
			if(isTRUE(check)) {
				self$comments <- c(self$comments, comment)
			} else {
				stop(check)
			}
		},
		#' @details
		#' convert \code{[commentable]} object into an R list of comments
		#' @return a list where the names are comment names and the values comment values
		to_list = function() {
			comments = purrr::map(self$comments$to_list())
			return(comments)
		},
		#' @details
		#' convert an R list into a object \code{[commentable]}
		#' @param comments a R list where the names are comment names and the values are comments
		#' @return commentable a \code{[commentable]} object
		from_list = function(comments) {
			commentable <- self$new(
				comments = purrr::imap(comments, ~comment$new(.y, .x))
			)
			return(commentable)
		}
	)
)
