test_that("error_with_check_message_on_failure works", {
	expect_error(error_with_check_message_on_failure("error text"), regexp = "error text")
	expect_true(error_with_check_message_on_failure(TRUE))
})

test_that("check_comments works", {
	expect_true(check_comments(NULL))
	expect_error(check_comments("string"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(check_comments(1L), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'integer'")
	expect_error(check_comments(1), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'double'")
	expect_error(check_comments(TRUE), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'logical'")
	expect_error(check_comments(list(name = c("string", "string2"))), regexp = "Comment\\(s\\) 1")
	expect_error(
		check_comments(list(a = "b", name = c("string", "string2"))),
		regexp = "Comment\\(s\\) 2"
	)
	expect_error(check_comments(list("string")), regexp = "Must have names")
	expect_true(check_comments(list(name = "string")))
	expect_true(error_with_check_message_on_failure(TRUE))
})
