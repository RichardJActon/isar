# OntologyAnnotation ----
test_that("OntologyAnnotation works", {
	test_ontology_annotation <- OntologyAnnotation$new()
	checkmate::expect_r6(test_ontology_annotation, "OntologyAnnotation")

	## set_term_source ----
	test_ontology_annotation$set_term_source(OM)

	expect_equal(OM, test_ontology_annotation$term_source)
	expect_error(test_ontology_annotation$set_term_source(NULL), regexp = "Must be an R6 class, not 'NULL'")

	## set_term ----
	test_ontology_annotation$set_term("gram")

	expect_equal(test_ontology_annotation$term, "gram")
	expect_equal(test_ontology_annotation$term_accession, "g")
	expect_error(test_ontology_annotation$set_term("flux capacitance"), regexp = "term is not in term source")

	test_ontology_annotation$set_term_accession("mg")
	expect_equal(test_ontology_annotation$term, "milligram")

	expect_error(OntologyAnnotation$new(term = "gram", term_source = OM, term_accession = "p"), regex = "Supplied term & term accession do not match!")

	## Comments ----
	test_comments(test_ontology_annotation)

	## ID ----
	id2test <- test_ontology_annotation$get_id()
	expect_true(test_id(id2test))
	id2test_with_suffix <- test_ontology_annotation$set_id(id2test, suffix = "post")
	expect_error(test_ontology_annotation$set_id(id2test, suffix = "-post"), regexp = "suffix must not contain any special characters")
	expect_error(test_ontology_annotation$set_id("notuuid"), regexp = "invalid uuid!")
	expect_true(test_id(id2test_with_suffix))
	expect_equal(paste0(id2test, "-post"), id2test_with_suffix)

	## To list ----
	# !! test recursive
	test_ontology_annotation <- OntologyAnnotation$new()

	test_ontology_annotation$set_term_source(OM)
	example_list <- list(
		id = test_ontology_annotation$get_id(),
		annotation_value = NULL,
		term_source = "Ontology of units of Measure (OM)",
		term_accession = NULL,
		comments = NULL
	)
	expect_equal(test_ontology_annotation$to_list(recursive = FALSE), example_list)

	## From list ----
	# !! test recursive
	test_from_list <- OntologyAnnotation$new()
	test_from_list$from_list(example_list, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list)

	example_list_null <- list(
		id = test_ontology_annotation$get_id(),
		annotation_value = NULL,
		term_source = NULL,
		term_accession = NULL,
		comments = NULL
	)
	test_from_list <- OntologyAnnotation$new()
	test_from_list$from_list(example_list_null, recursive = FALSE)
	expect_equal(test_from_list$to_list(recursive = FALSE), example_list_null)
})
