test_that("check_empty", {

	mode <- "character"
	text <- paste0("Must be empty ", mode, " vector")
	expect_true(check_empty(character(), mode))
	expect_true(check_empty(character(), mode, null.ok = TRUE))
	expect_true(check_empty(NULL, mode, null.ok = TRUE))
	expect_equal(check_empty("", mode), text)
	text <- paste0(text, " or NULL")
	expect_equal(check_empty("", mode, null.ok = TRUE), text)

	expect_true(test_empty(character(), mode))
	expect_true(test_empty(character(), mode, null.ok = TRUE))
	expect_true(test_empty(NULL, mode, null.ok = TRUE))
	expect_false(test_empty("", mode))
	expect_false(test_empty("", mode, null.ok = TRUE))

	expect_empty(character(), mode)
	expect_empty(character(), mode, null.ok = TRUE)
	expect_empty(NULL, mode, null.ok = TRUE)

	mode <- "logical"
	text <- paste0("Must be empty ", mode, " vector")
	expect_true(check_empty(logical(), mode))
	expect_true(check_empty(logical(), mode, null.ok = TRUE))
	expect_true(check_empty(NULL, mode, null.ok = TRUE))
	expect_equal(check_empty(NA, mode), text)
	text <- paste0(text, " or NULL")
	expect_equal(check_empty(NA, mode, null.ok = TRUE), text)

	expect_true(test_empty(logical(), mode))
	expect_true(test_empty(logical(), mode, null.ok = TRUE))
	expect_true(test_empty(NULL, mode, null.ok = TRUE))
	expect_false(test_empty(NA, mode))
	expect_false(test_empty(NA, mode, null.ok = TRUE))

	expect_empty(logical(), mode)
	expect_empty(logical(), mode, null.ok = TRUE)
	expect_empty(NULL, mode, null.ok = TRUE)

	mode <- "numeric"
	text <- paste0("Must be empty ", mode, " vector")
	expect_true(check_empty(numeric(), mode))
	expect_true(check_empty(numeric(), mode, null.ok = TRUE))
	expect_true(check_empty(NULL, mode, null.ok = TRUE))
	expect_equal(check_empty(1, mode), text)
	text <- paste0(text, " or NULL")
	expect_equal(check_empty(c(1,3), mode, null.ok = TRUE), text)

	expect_true(test_empty(numeric(), mode))
	expect_true(test_empty(numeric(), mode, null.ok = TRUE))
	expect_true(test_empty(NULL, mode, null.ok = TRUE))
	expect_false(test_empty(1, mode))
	expect_false(test_empty(c(1,3), mode, null.ok = TRUE))

	expect_empty(numeric(), mode)
	expect_empty(numeric(), mode, null.ok = TRUE)
	expect_empty(NULL, mode, null.ok = TRUE)
})
