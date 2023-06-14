test_that("Publication works", {
	test_publication <- Publication$new()
	## Comments ----
	test_comments(test_publication)
	
	## Authors ----
	expect_null(test_publication$to_list()$author_list)

	expect_true(test_publication$check_doi("10.1016/j.cell.2021.04.013"))
	expect_error(test_publication$check_doi("23453/10.1016/j.cell.2021.04.013"), regexp = "Invalid DOI")
	expect_error(test_publication$set_doi("23453/10.1016/j.cell.2021.04.013"), regexp = "Invalid DOI")

})
