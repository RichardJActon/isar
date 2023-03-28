test_that("Characteristic works", {

	drug <- OntologySource$new("drug", terms_list = list(dox = "dox", flox = "flox", pox = "pox"))

	exposure_1 <- OntologyAnnotation$new("dox", drug)
	# exposure_2 <- OntologyAnnotation$new("flox", drug)

	mg <- Unit$new(unit = "milligram")

	test_characteristic <- Characteristic$new(
		category = exposure_1, value = 10, unit = mg
	)
	checkmate::expect_r6(test_characteristic, "Characteristic")

	# Comments
	expect_true(test_characteristic$check_comments(list("a" = "b")))
	expect_true(test_characteristic$check_comments(list("a" = "1", "b" = "2")))

	expect_error(test_characteristic$check_comments(list("b")), regexp = "Must have names")
	expect_error(test_characteristic$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(test_characteristic$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")

	expect_error(test_characteristic$set_comments(list("b")), regexp = "Must have names")
	expect_error(test_characteristic$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")

	#
	expect_true(uuid::UUIDvalidate(test_characteristic$get_id()))

	test_characteristic$set_comments(list("a" = "1", "b" = "2"))
	test_characteristic$to_list()

	# identity tests
	expect_true(identical(test_characteristic, test_characteristic))
	expect_false(identical(test_characteristic, Characteristic$new()))
})
