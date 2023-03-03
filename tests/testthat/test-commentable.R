test_that("commentable works", {
	test_commentable <- commentable$new()
	checkmate::expect_r6(test_commentable, "commentable")

	#list(comments$new())
})
