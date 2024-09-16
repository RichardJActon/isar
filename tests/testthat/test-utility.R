# error_with_check_message_on_failure ----
test_that("error_with_check_message_on_failure works", {
	expect_error(error_with_check_message_on_failure("error text"), regexp = "error text")
	expect_true(error_with_check_message_on_failure(TRUE))
})

# check_comments ----
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

# date string conversion ---
test_that("date_string_conversion works",{
	# Simple working
	expect_equal(date_input_handling("1990-06-26"), as.Date("1990-06-26"))
	expect_equal(as.character(date_input_handling("1990-06-26")), "1990-06-26")
	# not a leap year but feb 29
	expect_error(
		warns <- capture_warnings(date_input_handling("3034-02-29")),
		regexp = "No Valid Date format found"
	)
	warns <- capture_warnings(date_input_handling("10/12/1928"))

})

# S3? ----

# custom uuid functions ----
test_that("generate_id, test_id & check_id work", {
	id2test <- generate_id()
	expect_true(uuid::UUIDvalidate(id2test))
	expect_true(test_id(id2test))
	id_with_suffix2test <- generate_id(suffix = "x")
	expect_false(uuid::UUIDvalidate(id_with_suffix2test))
	expect_true(test_id(id_with_suffix2test))
	expect_error(generate_id(suffix = "-x"), regexp = "suffix must not contain any special characters")
	expect_error(generate_id("notauuid"), regexp = "invalid uuid!")
	expect_error(test_id(1), regexp = "input must be a character vector")
	expect_equal(check_id("notauuid"), "id must be a valid uuid or a valid uuid with a suffix")
	expect_true(check_id(id2test))
	expect_true(check_id(id_with_suffix2test))
})
