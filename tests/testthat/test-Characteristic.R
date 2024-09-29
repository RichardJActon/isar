# Characteristic ----
test_that("Characteristic works", {

	drug <- OntologySource$new(
		"drug", terms_list = list(dox = "dox", flox = "flox", pox = "pox")
	)

	ont_refs <- OntologySourceReferences$new(
		list("drug" = drug, "Ontology of units of Measure (OM)" = OM)
	)
	exposure_1 <- OntologyAnnotation$new(
		"dox", drug, ontology_source_references = ont_refs
	)

	milligram <- OntologyAnnotation$new(
		"milligram", OM, ontology_source_references = ont_refs
	)
	
	mg <- Unit$new(
		`@id` = "#unit/milligram", unit = milligram,
		ontology_source_references = ont_refs
	)

	charcat_1_chemcomp <- CharacteristicCategory$new(
		`@id` = "#charcat/example", origin = "example",
		type = exposure_1,
		ontology_source_references = ont_refs
	)
	
	test_characteristic <- Characteristic$new(
		category = charcat_1_chemcomp, value = 10, unit = mg
	)
	checkmate::expect_r6(test_characteristic, "Characteristic")

	## Comments ----
	test_comments(test_characteristic)

	test_characteristic$set_comments(list("a" = "1", "b" = "2"))

	## To list ----
	test_characteristic <- Characteristic$new()
	test_characteristic$set_category(charcat_1_chemcomp)
	test_characteristic$set_unit(mg)
	test_characteristic$set_value(10)


	example_list <- list(
		category = list(`@id` = "#charcat/example"),
		unit = list(
			`@id` = "#unit/milligram",
			termAccession = "mg",
			annotationValue = "milligram",
			termSource = "Ontology of units of Measure (OM)"
		),
		value = 10# ,
		# comments = NULL
	)
	expect_equal(test_characteristic$to_list(), example_list)

	## From list ----
	urefs <- UnitReferences$new(ontology_source_references = ont_refs)
	urefs$add_unit_references(list("#unit/milligram" = mg))
	
	test_from_list <- Characteristic$new(
		ontology_source_references = ont_refs, unit_references = urefs,
		category_references = CharacteristicCategoryReferences$new(
			list("#charcat/example" = charcat_1_chemcomp)
		)
	)
	test_from_list$from_list(example_list)

	expect_equal(test_from_list$to_list(), example_list)

	# identity tests
})
