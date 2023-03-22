#' check_comments
#'
#' @param comments a list of comments, named and with length 1 character vectors#
#' @importFrom checkmate check_list test_string
#' @importFrom purrr map_lgl
check_comments <- function(comments) {
	check <- checkmate::check_list(
		comments, min.len = 1, types = "character", names = "named",
		null.ok = TRUE
	)
	if (!isTRUE(check)) {
		stop(check)
	} else {
		comment_is_string <- purrr::map_lgl(
			comments, ~checkmate::test_string(.x)
		)
		if(all(comment_is_string)) {
			return(TRUE)
		} else {
			stop(paste0(
				"Comment(s) ",
				paste0(which(!comment_is_string), collapse = ", "),
				"\n Are not Strings (character vectors of length 1)"
			))
		}
	}
}

#' error_with_check_message_on_failure
#'
#' Used in conjunction with checkmate check_* functions
#' has the behavior of returning TRUE if a check is TRUE and throwing an error
#' if check produces a message.
#'
#' assertions don't return TRUE on success but do error on failure and tests
#' don't produce any information about the reason for failure.
#' Checks produce a reasons on failure but don't throw an error.
#'
#' @param check the output from a check_* function for the checkmate package
#' @param nextline text to put on the line after check message
error_with_check_message_on_failure <- function(check, nextline = NULL) {
	if(isTRUE(check)) { return(TRUE) } else {
		if(!is.null(nextline)) { check <- paste0(check, "\n", nextline) }
		stop(check)
	}
}

#' s3_identical_maker
#'
#' function to aid in the creation of S3 generic methods for \code{[identical]}
#' or R6 objects
#' @param obj_pub_props a chacater vector of public properties whose values
#' should be compared to determine identity
#' @param get_id  (default = TRUE) Compare the object's unique identifiers
#' using the get_id method
s3_identical_maker <- function(obj_pub_props, get_id = TRUE) {
	function(x, y) {
		obj_pub_props <- obj_pub_props
		obj_pub_props_lgl <- purrr::map_lgl(
			obj_pub_props, ~identical(x[[.x]], y[[.y]])
		)
		res <- NULL
		if(get_id) {
			res <- all(obj_pub_props_lgl, identical(x$get_id(), y$get_id()))
		} else {
			res <- all(obj_pub_props_lgl)
		}
		return(res)
	}
}
