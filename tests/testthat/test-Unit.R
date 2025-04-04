test_that("Unit Object works", {
	# empty object defaults
	obj <- Unit$new()
	expect_true(is.na(obj$origin))
	expect_empty(obj$`@id`, mode = "character")
	expect_r6(obj$ontology_source_references, "OntologySourceReferences")

	# Unit information as a list, without a pre-defined ontology source
	lst <- list(
		`@id` = "#unit/ms^-2",
		annotationValue = "ms^-2",
		termAccession = "meterspersecondsquared",
		termSource = "AUnitOntology"
	)
	expect_warning(obj$from_list(lst), "Term Source Unknown")

	# Adding an Ontology Source, without any terms
	obj <- Unit$new()
	obj$ontology_source_references$from_list(
		list(list(name = "AUnitOntology", version = "0.0.0"))
	)
	expect_warning(obj$from_list(lst), "Term not in source")
	# default origin
	expect_true(is.na(obj$origin))

	# Adding a term when one has been inferred
	expect_warning(
		obj$ontology_source_references$ontology_source_references$AUnitOntology$add_terms(
			terms_list = list("ms^-2" = "meterspersecondsquared")
		),
		"These terms already exist"
	)

	# Unit with a source a term in that source
	obj <- Unit$new()
	obj$ontology_source_references$from_list(
		list(list(name = "AUnitOntology", version = "0.0.0")),
		origin = "manual"
	)
	obj$ontology_source_references$ontology_source_references$AUnitOntology$add_terms(
		terms_list = list("ms^-2" = "meterspersecondsquared")
	)
	obj$from_list(lst)
	expect_equal(obj$unit$term_source$name, "AUnitOntology")
	# specified origin
	expect_equal(obj$unit$term_source$origin, "manual")
})
