#' test_comments
#'
#' Perform multiple standard tests on objects which use the comment methods:
#' \code{check_comments}, \code{set_comments}, & \code{add_comment}
#'
#' @param obj an object with comments
#' @importFrom testthat expect_true expect_error
test_comments <- function(obj) {
	expect_true(obj$check_comments(list(list(name = "a", value = "a"))))
	expect_true(obj$check_comments(list(list(name = "a", value = "a"), list(name = "b", value = "b"))))

	expect_error(obj$check_comments(list(list(name = "b", x = "b"))), regexp = "A 'comment' must be list with names 'value' & 'name'")
	expect_error(obj$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(obj$check_comments(list(list(name = "a", value = 1L))), regexp = "May only contain the following types: \\{character\\}, but element 2 has type 'integer'")

	expect_error(obj$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
}
