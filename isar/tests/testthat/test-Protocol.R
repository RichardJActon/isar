# Protocol ----
test_that("Protocol works", {
	test_protocol <- Protocol$new()

	## Comments ----
	expect_true(test_protocol$check_comments(list("a" = "b")))
	expect_true(test_protocol$check_comments(list("a" = "1", "b" = "2")))

	expect_error(test_protocol$check_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(test_protocol$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")

	expect_error(test_protocol$set_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")

	## ID ----
	expect_true(uuid::UUIDvalidate(test_protocol$get_id()))

	## To list ----
	example_list <- list(
		name = "DNA isolation",
		id = test_protocol$get_id(),
		component_type = "gram",
		comments = NULL
	)
	expect_equal(test_protocol$to_list(recursive = FALSE), example_list)

	## From list ----
	test_from_list <- ProtocolComponent$new()
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
