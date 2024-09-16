test_that("Source works", {
	test_source <- Source$new()
	checkmate::expect_r6(test_source, "Source")
})
