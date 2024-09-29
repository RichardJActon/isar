# OntologyAnnotation ----
test_that("OntologyAnnotation works", {
	osr <- OntologySourceReferences$new()
	OM$explicitly_provided <- TRUE
	osr$add_ontology_sources(list("Ontology of units of Measure (OM)" = OM))

	test_ontology_annotation <- OntologyAnnotation$new(
		ontology_source_references = osr
	)
	checkmate::expect_r6(test_ontology_annotation, "OntologyAnnotation")

	## set_term_source ----
	test_ontology_annotation$set_term_source(OM)

	expect_equal(OM, test_ontology_annotation$term_source)
	expect_error(
		test_ontology_annotation$set_term_source(NULL),
		regexp = "Must be an R6 class, not 'NULL'"
	)

	## set_term ----
	test_ontology_annotation$set_term("gram")

	expect_equal(test_ontology_annotation$term, "gram")
	expect_equal(test_ontology_annotation$term_accession, "g")
	expect_error(
		test_ontology_annotation$set_term("flux capacitance"),
		regexp = "term is not in term source"
	)

	test_ontology_annotation$set_term_accession("mg")
	expect_equal(test_ontology_annotation$term, "milligram")

	expect_error(
		OntologyAnnotation$new(
			term = "gram", term_source = OM, term_accession = "p",
			ontology_source_references = osr
		),
		regex = "Supplied term & term accession do not match!"
	)

	## Comments ----
	test_comments(test_ontology_annotation)

	## To list ----
	# !! test recursive
	test_ontology_annotation <- OntologyAnnotation$new(
		ontology_source_references = osr
	)

	test_ontology_annotation$set_term_source(OM)
	example_list <- list(
		termSource = "Ontology of units of Measure (OM)"
	)
	expect_equal(test_ontology_annotation$to_list(), example_list)

	## From list ----
	example_list <- list(
		termAccession = "mm",
		annotationValue = "millimetre",
		termSource = "Ontology of units of Measure (OM)"
	)
	test_from_list <- OntologyAnnotation$new(ontology_source_references = osr)
	test_from_list$from_list(example_list)
	expect_equal(test_from_list$to_list(), example_list)

})
