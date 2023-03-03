test_that("comments work", {
	test_comment <- comment$new()
	checkmate::expect_r6(test_comment, "comment")

	ex_name <- "example"
	ex_value <- "this is a comment"
	test_comment <- comment$new(ex_name, ex_value)

	string_not_double <- "Must be of type 'string', not 'double'"
	expect_error(test_comment$check_name(1), regexp = string_not_double)
	expect_error(test_comment$check_value(1), regexp = string_not_double)
	expect_error(test_comment$set_name(1), regexp = string_not_double)
	expect_error(test_comment$set_value(1), regexp = string_not_double)

	string_not_logical <- "Must be of type 'string', not 'logical'"
	expect_error(test_comment$check_name(TRUE), regexp = string_not_logical)
	expect_error(test_comment$check_value(TRUE), regexp = string_not_logical)
	expect_error(test_comment$set_name(TRUE), regexp = string_not_logical)
	expect_error(test_comment$set_value(TRUE), regexp = string_not_logical)

	ex_list <- list(ex_value)
	names(ex_list) <- ex_name
	expect_equal(test_comment$to_list(), ex_list)

	test_from_list <- comment$new()
	test_from_list$from_list(ex_list)

	checkmate::expect_r6(test_from_list, "comment")
	expect_equal(test_from_list$name, ex_name)
	expect_equal(test_from_list$value, ex_value)

	expect_error(
		test_from_list$from_list(list()),
		regexp = "Must have length 1, but has length 0"
	)
	expect_error(
		test_from_list$from_list(list(a = "b", b = "c")),
		regexp = "Must have length 1, but has length 2"
	)
	expect_error(
		test_from_list$from_list(list("a")),
		regexp = "Must have names"
	)
	expect_error(
		test_from_list$from_list(list(a = 1)),
		regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'numeric'"
	)
})
