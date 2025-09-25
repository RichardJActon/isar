# ProtocolParameter ----
test_that("ProtocolParameter works", {
	test_protocol_parameter <- ProtocolParameter$new()

	## Parameter Name ----
	# expect_null(test_protocol_parameter$parameter_name)
	checkmate::expect_r6(
		test_protocol_parameter$parameter_name, "OntologyAnnotation"
	)

	warns <- capture_warnings(
		oa <- OntologyAnnotation$new(term = "gram", term_source = OM)
	)
	expect_match(warns, "Attempting to add it", all = FALSE)

	warns <- capture_warnings(test_protocol_parameter$set_parameter_name(oa))
	expect_match(warns, "Attempting to add it", all = FALSE)

	checkmate::expect_r6(test_protocol_parameter$parameter_name, "OntologyAnnotation")

	## Comments ----
	test_comments(test_protocol_parameter)

	## To list ----
	example_list <- list(
		parameterName = list(
			termAccession = "g",
			annotationValue = "gram",
			termSource = "Ontology of units of Measure (OM)"
		),
		# comments = NULL,
		`@id` = character()
	)
	expect_equal(test_protocol_parameter$to_list(), example_list)

	test_from_example_list <- ProtocolParameter$new()
	warns <- capture_warnings(test_from_example_list$from_list(example_list))

	example_list_missing <- list(
		parameterName = list(
			annotationValue = "gram"
		),
		# comments = NULL,
		`@id` = character()
	)
	test_from_list_missing <- ProtocolParameter$new()
	warns <- capture_warnings(test_from_list_missing$from_list(example_list_missing))
	expect_equal(test_from_list_missing$to_list(), example_list_missing)

	#From list ----
	# without OM in the reference
	test_from_list <- ProtocolParameter$new()
	warns <- capture_warnings(test_from_list$from_list(example_list))
	expect_match(warns, "Term Source Unknown")
	expect_equal(test_from_list$to_list(), example_list)

	# with OM in the reference
	ors <- OntologySourceReferences$new()
	ors$add_ontology_source(OM)
	test_from_list <- ProtocolParameter$new(ontology_source_references = ors)
	test_from_list$from_list(example_list)
	expect_equal(test_from_list$to_list(), example_list)

})
