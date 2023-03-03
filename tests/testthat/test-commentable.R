test_that("commentable works", {
	# New commentable object with no arguments
	test_commentable <- commentable$new()
	checkmate::expect_r6(test_commentable, "commentable")
	expect_null(test_commentable$comments)

	test_commentable <- commentable$new(list(comment$new("a","comment")))

	comment_check_error_text <- "All comments must be comment objects"

	# new fails with bad comment lists
	expect_error(commentable$new(list()), regexp = comment_check_error_text)

	# expect_error(commentable$new(NULL), regexp = comment_check_error_text)
	expect_error(
		commentable$new(list(comment$new(), "string")),
		regexp = comment_check_error_text
	)

	# check_comments fails with bad comment lists
	expect_error(test_commentable$check_comments(list()), regexp = comment_check_error_text)
	expect_error(test_commentable$check_comments(NULL), regexp = comment_check_error_text)
	expect_error(test_commentable$check_comments(list(comment$new(), "string")), regexp = comment_check_error_text)

	# set_comments fails with bad comment lists
	expect_error(test_commentable$set_comments(list()), regexp = comment_check_error_text)
	expect_error(test_commentable$set_comments(NULL), regexp = comment_check_error_text)
	expect_error(test_commentable$set_comments(list(comment$new(), "string")), regexp = comment_check_error_text)

	expect_equal(length(test_commentable$get_comments()), 1L)
	test_commentable$add_comment(comment$new("append","me"))
	expect_equal(length(test_commentable$get_comments()), 2L)

	expect_true(all(purrr::map_lgl(
		test_commentable$get_comments(), ~checkmate::test_r6(.x,"comment")
	)))

	expect_equal(test_commentable$get_comment_names(), c("a", "append"))
	expect_equal(test_commentable$get_comment_values(), c("comment", "me"))

	expect_equal(test_commentable$get_comment("append")[[1]]$name, "append")

	expect_error(
		test_commentable$get_comment("fake"), regexp = "No comment by this name"
	)

	expect_equal(
		test_commentable$to_tidy(),
		tibble::tibble(name = c("a","append"), value = c("comment", "me"))
	)

	cl <- test_commentable$to_list()
	expect_equal(cl, list(a = "comment", append = "me"))

	commentable_from_list <- commentable$new()
	commentable_from_list$from_list(cl)
	expect_equal(commentable_from_list$to_list(), cl)

})
