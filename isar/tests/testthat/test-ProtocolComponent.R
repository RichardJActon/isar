test_that("ProtocolComponent works", {
	test_protocol_component <- ProtocolComponent$new()

	# Name ----
	expect_error(test_protocol_component$set_name(""), regex = "All elements must have at least 1 characters, but element 1 has 0 characters")
	expect_error(test_protocol_component$set_name(1), regex = "Must be of type 'string', not 'double'")
	expect_equal(test_protocol_component$set_name("DNA isolation"), test_protocol_component$name)

	# Component Type
	oa <- OntologyAnnotation$new("gram", term_source = OM)
	expect_true(test_protocol_component$check_component_type(oa))

	test_protocol_component$set_component_type(oa)
	expect_equal(oa, test_protocol_component$component_type)

	expect_error(test_protocol_component$set_component_type(NULL),  regexp = "Must be an R6 class, not 'NULL'")
	expect_error(test_protocol_component$set_component_type(""), regexp = "Must be an R6 class, not character")

	# Comments ----
	expect_true(test_protocol_component$check_comments(list("a" = "b")))
	expect_true(test_protocol_component$check_comments(list("a" = "1", "b" = "2")))

	expect_error(test_protocol_component$check_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol_component$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(test_protocol_component$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")

	expect_error(test_protocol_component$set_comments(list("b")), regexp = "Must have names")
	expect_error(test_protocol_component$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")

	# ID ----
	expect_true(uuid::UUIDvalidate(test_protocol_component$get_id()))

	# To list ----
	example_list <- list(
		name = "DNA isolation",
		id = test_protocol_component$get_id(),
		component_type = "gram",
		comments = NULL
	)
	expect_equal(test_protocol_component$to_list(recursive = FALSE), example_list)

	# From list ----
	test_from_list <- ProtocolComponent$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	example_list_null <- list(
		name = "DNA isolation",
		id = test_protocol_component$get_id(),
		component_type = NULL,
		comments = NULL
	)
	test_from_list <- ProtocolComponent$new()
	test_from_list$from_list(example_list_null, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list_null)

	# warning from OntologySource from list that needs resolving
	# recursive_list <- test_protocol_component$to_list(recursive = TRUE)
	# test_from_list <- ProtocolComponent$new()
	# test_from_list$from_list(recursive_list, recursive = TRUE)

})
