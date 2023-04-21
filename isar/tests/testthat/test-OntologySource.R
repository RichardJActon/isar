# OntologySource ----
test_that("OntologySource works", {
	test_ontology_source <- OntologySource$new()

	## Comments ----
	expect_true(test_ontology_source$check_comments(list("a" = "b")))
	expect_true(test_ontology_source$check_comments(list("a" = "1", "b" = "2")))

	expect_error(test_ontology_source$check_comments(list("b")), regexp = "Must have names")
	expect_error(test_ontology_source$check_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")
	expect_error(test_ontology_source$check_comments(list("a" = 1L)), regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'integer'")

	expect_error(test_ontology_source$set_comments(list("b")), regexp = "Must have names")
	expect_error(test_ontology_source$set_comments("b"), regexp = "Must be of type 'list' \\(or 'NULL'\\), not 'character'")

	## get_terms_list ----
	expect_error(OntologySource$new(get_terms_list = function() { list(a=1,b=2) }), regexp = "unused arguments")
	# Function must have the correct parameters
	checkmate::expect_r6(OntologySource$new(get_terms_list = function(file, file_type, url, version){list(a=1,b=2)}), "OntologySource")

	## list conversion ----
	test_to_list <- test_ontology_source$to_list()

	expect_true(is.list(test_to_list))

	test_from_list <- OntologySource$new()
	test_from_list$from_list(test_to_list)

	checkmate::expect_r6(test_from_list, "OntologySource")
})
