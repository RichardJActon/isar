test_that("Assay works", {
	test_assay <- Assay$new()
	
	## Comments ----
	test_comments(test_assay)
	
	techtype <- OntologySource$new("techtype", terms_list = list(Human = "clay tablet", Alien = "£$&%$%^&*"))
	cowsphericity <- OntologySource$new("cowsphericity", terms_list = list(High = "Physics", Low = "Biology"))

	ont_refs <- OntologySourceReferences$new(
		list("techtype" = techtype, "cowsphericity" = cowsphericity)
	)
		
	human <- OntologyAnnotation$new("Human", techtype,ontology_source_references = ont_refs)
	biology <- NULL
	warns <- capture_warnings(
		biology <- OntologyAnnotation$new("Low", cowsphericity)
	)
	expect_match(warns, "is not in the ontology source reference")
	test_assay$set_technology_type(human)
	expect_equal(test_assay$technology_type$term, "Human")
	test_assay$set_measurement_type(biology)
	expect_equal(test_assay$measurement_type$term, "Low")

	expect_true(test_assay$check_technology_type(human))
	expect_error(test_assay$check_technology_type("human"), regexp = "Must be an R6 class, not character")
	expect_true(test_assay$check_measurement_type(biology))
	expect_error(test_assay$check_measurement_type("biology"), regexp = "Must be an R6 class, not character")

})
