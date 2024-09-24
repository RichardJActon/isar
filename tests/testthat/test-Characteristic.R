# Characteristic ----
test_that("Characteristic works", {

	drug <- OntologySource$new("drug", terms_list = list(dox = "dox", flox = "flox", pox = "pox"))

	ont_refs <- OntologySourceReferences$new(
		list("drug" = drug)
	)
	exposure_1 <- OntologyAnnotation$new("dox", drug, ontology_source_references = ont_refs)
	# exposure_2 <- OntologyAnnotation$new("flox", drug)

	mg <- Unit$new(unit = "milligram")

	test_characteristic <- Characteristic$new(
		category = exposure_1, value = 10, unit = mg
	)
	checkmate::expect_r6(test_characteristic, "Characteristic")

	## Comments ----
	test_comments(test_characteristic)

	## ID ----
	# expect_true(uuid::UUIDvalidate(test_characteristic$get_id()))

	test_characteristic$set_comments(list("a" = "1", "b" = "2"))
	# test_characteristic$to_list(recursive = TRUE)

	## To list ----
	# !! test recursive
	test_characteristic <- Characteristic$new()
	test_characteristic$set_category(exposure_1)
	test_characteristic$set_unit(mg)
	test_characteristic$set_value(10)


	example_list <- list(
		id = test_characteristic$get_id(),
		category = "dox",
		value = 10,
		unit = "milligram",
		comments = NULL
	)
	expect_equal(test_characteristic$to_list(recursive = FALSE), example_list)

	## From list ----
	# !! test recursive
	test_from_list <- OntologyAnnotation$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	# expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	test_from_list <- Characteristic$new(
		category = exposure_1, value = 10, unit = mg
	)

	example_list_null <- list(
		id = test_characteristic$get_id(),
		annotation_value = NULL,
		term_source = NULL,
		term_accession = NULL,
		comments = NULL
	)
	test_from_list <- Characteristic$new()
	test_from_list$from_list(example_list_null, recursive = FALSE)
	# expect_equal(test_from_list$to_list(recursive = FALSE), example_list_null)

	# identity tests
	# expect_true(identical(test_characteristic, test_characteristic))
	# expect_false(identical(test_characteristic, Characteristic$new()))
})
