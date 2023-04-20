test_that("ProtocolParameter works", {
	test_protocol_parameter <- ProtocolParameter$new()

	# Parameter Name ----
	expect_null(test_protocol_parameter$parameter_name)

	oa <- OntologyAnnotation$new(term = "gram", term_source = OM)
	test_protocol_parameter$set_parameter_name(oa)
	checkmate::expect_r6(test_protocol_parameter$parameter_name, "OntologyAnnotation")

	# Comments ----
	expect_true(test_protocol_parameter$check_comments(list("a" = "b")))
	expect_true(test_protocol_parameter$check_comments(list("a" = "1", "b" = "2")))

	expect_error(test_protocol_parameter$check_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol_parameter$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(test_protocol_parameter$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")

	expect_error(test_protocol_parameter$set_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol_parameter$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")

	# ID ----
	expect_true(uuid::UUIDvalidate(test_protocol_parameter$get_id()))

	# To list ----
	example_list <- list(
		id = test_protocol_parameter$get_id(),
		parameter_name = "gram",
		comments = NULL
	)
	expect_equal(test_protocol_parameter$to_list(recursive = FALSE), example_list)

	# From list ----
	test_from_list <- ProtocolParameter$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	# Identity ----

	# pp1 <- ProtocolParameter$new()
	# pp1a <- pp1
	# pp2 <- ProtocolParameter$new()
	# expect_true(identical(pp1, pp1a))
	# expect_false(identical(pp1, pp2))
})
