test_that("factor_values checking works", {
	test_sample <- Sample$new()
	## Comments ----
	test_comments(test_sample)
	
	## factor value ----
	expect_error(
		test_sample <- Sample$new(
			name = "test",
			factor_values = "string"
		),
		regexp = "factor_values is not a list"
	)
	expect_error(
		test_sample <- Sample$new(
			name = "test",
			factor_values = list("")
		),
		regexp = "Not all factor_values are factor_value objects"
	)
})


# test_that("List serialization works", {
#
# })
