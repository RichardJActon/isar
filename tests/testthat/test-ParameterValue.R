test_that("ParameterValue works", {
	test_parameter_value <- ParameterValue$new()
	## Comments ----
	test_comments(test_parameter_value)

	example_list <- list(
		value = 0.22,
		unit = list(`@id` = "#Unit/micrometer"),
		category = list(`@id` = "#parameter/filter_pore_size"),
		comments = list(list(name = "test", value = "comment"))
	)

	warns <- capture_warnings(test_parameter_value$from_list(example_list))
	# expect_match(warns, "Missing term accession", all = FALSE)
	# expect_match(warns, "Term not in source", all = FALSE)
	# expect_match(warns, "Term Source Unknown", all = FALSE)
	# expect_match(warns, "Unspecified Term", all = FALSE)

	expect_r6(
		test_parameter_value$protocol$parameters[[
			"#parameter/filter_pore_size"
		]],
		"ProtocolParameter"
	)
	expect_r6(test_parameter_value$protocol_references, "ProtocolReferences")
	expect_r6(
		test_parameter_value$protocol$protocol_references,
		"ProtocolReferences"
	)
	expect_r6(test_parameter_value$protocol, "Protocol")
	expect_equal(
		test_parameter_value$protocol$parameters[[
			"#parameter/filter_pore_size"
		]]$`@id`,
		example_list$category$`@id`
	)

})


# You cannot create a [ParameterValue] object without providing a [Protocol] object!
# A [Process] entails the execution of a [Protocol].
# [Protocol]s have parameters the values of which ([ParameterValue]s) can be specified in a [Process] executing that [Protocol].
# The 'categories' of [ParameterValue]s available are exposed by the [Protocol].
# Thus in order to know the available categories of [ParameterValue]s the [Protocol] being executed must be specified.
