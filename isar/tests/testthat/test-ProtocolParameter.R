# ProtocolParameter ----
test_that("ProtocolParameter works", {
	test_protocol_parameter <- ProtocolParameter$new()

	## Parameter Name ----
	expect_null(test_protocol_parameter$parameter_name)

	oa <- OntologyAnnotation$new(term = "gram", term_source = OM)
	test_protocol_parameter$set_parameter_name(oa)
	checkmate::expect_r6(test_protocol_parameter$parameter_name, "OntologyAnnotation")

	## Comments ----
	test_comments(test_protocol_parameter)

	## ID ----
	expect_true(uuid::UUIDvalidate(test_protocol_parameter$get_id()))

	## To list ----
	example_list <- list(
		id = test_protocol_parameter$get_id(),
		parameter_name = "gram",
		comments = NULL
	)
	expect_equal(test_protocol_parameter$to_list(recursive = FALSE), example_list)

	## From list ----
	test_from_list <- ProtocolParameter$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	## Identity ----

	# pp1 <- ProtocolParameter$new()
	# pp1a <- pp1
	# pp2 <- ProtocolParameter$new()
	# expect_true(identical(pp1, pp1a))
	# expect_false(identical(pp1, pp2))
})
