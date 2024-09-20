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
		name = "DNA isolation",
		id = test_protocol$get_id(),
		protocol_type = NULL,
		description = character(),
		uri = character(),
		version = character(),
		parameters = NULL,
		components = NULL,
		comments = NULL
	)
	expect_equal(test_protocol$to_list(recursive = FALSE), example_list)

	## From list ----
	test_from_list <- Protocol$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	example_list_null <- list(
		name = "DNA isolation",
		id = test_protocol$get_id(),
		component_type = NULL,
		comments = NULL
	)
	test_from_list <- ProtocolComponent$new()
	test_from_list$from_list(example_list_null, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list_null)



})
