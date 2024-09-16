#' test_comments
#'
#' Perform multiple standard tests on objects which use the comment methods:
#' \code{check_comments}, \code{set_comments}, & \code{add_comment}
#'
#' @param obj an object with comments
#' @importFrom testthat expect_true expect_error
test_comments <- function(obj) {
	expect_true(obj$check_comments(list("a" = "b")))
	expect_true(obj$check_comments(list("a" = "1", "b" = "2")))
	
	expect_error(obj$check_comments(list("b")), regexp = "Must have names")
	expect_error(obj$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(obj$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")
	
	expect_error(obj$set_comments(list("b")), regexp = "Must have names")
	expect_error(obj$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
}
