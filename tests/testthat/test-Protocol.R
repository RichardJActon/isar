# Protocol ----
test_that("Protocol works", {
	test_protocol <- Protocol$new()

	## Comments ----
	test_comments(test_protocol)

	## ID ----
	# expect_true(uuid::UUIDvalidate(test_protocol$get_id()))

	test_protocol$set_name("DNA isolation")
	
	## To list ----
	example_list <- list(
		parameters = list(),
		components = list(),
		uri = character(),
		description = character(),
		version = character(),
		`@id` = "#protocol/",
		name = "DNA isolation",
		# protocolType = list()# ,
		protocolType = list(annotationValue = "")# ,
		# comments = NULL
	)
	expect_equal(test_protocol$to_list(), example_list)

	## From list ----
	test_from_list <- Protocol$new()
	
	warns <- capture_warnings(test_from_list$from_list(example_list))
	expect_match(warns, "Unspecified Term", all = FALSE)
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	
	expect_equal(test_from_list$to_list(), example_list)
	
	example_list_null <- list(
		parameters = NULL,
		components = NULL,
		uri = NULL,
		description = NULL,
		version = NULL,
		`@id` = NULL,
		name = NULL,
		protocolType = NULL, 
		comments = NULL
	)
	test_from_list <- Protocol$new()
	warns <- capture_warnings(test_from_list$from_list(example_list_null))
	expect_match(warns, "Unspecified Term", all = FALSE)
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	
	expect_null_out_lst <- list(
		parameters = list(),
		components = list(),
		protocolType = list(annotationValue = "")
	)
	expect_equal(test_from_list$to_list(), expect_null_out_lst)
	
})
