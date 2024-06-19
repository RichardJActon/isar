BII_I_1_jsonlite <- jsonlite::read_json(
	fs::path_abs(fs::path_join(c(
		testthat::test_path(), "test-data/ISAdatasets/json/BII-I-1/BII-I-1.json"
	)))
)
BII_S_3_jsonlite <- jsonlite::read_json(
	fs::path_abs(fs::path_join(c(
		testthat::test_path(), "test-data/ISAdatasets/json/BII-S-3/BII-S-3.json"
	)))

)
test_uuid <- uuid::UUIDgenerate()

# Ontology Source References ----
# inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)
# inv$get_ontology_source_names()
# detecting missing ontology sources
## Roles

# BII_I_1_jsonlite[["people"]][[1]][["roles"]]
# BII_I_1_jsonlite[["studies"]][[1]][["people"]][[1]][["roles"]]


# OntologyAnnotation ----
test_that("OntologyAnnotation", {
	obj <- OntologyAnnotation$new()
	ex <- BII_I_1_jsonlite$publications[[1]]$status
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	expect_match(warns, "Missing term accession", all = FALSE)

	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})


# OntologySource ----
test_that("OntologySource", {
	obj <- OntologySource$new()
	ex <- BII_I_1_jsonlite$ontologySourceReferences[[1]]
	obj$from_list(ex)
	expect_equal(obj$to_list(), ex)
})

# OntologySourceReferences ----
test_that("OntologySourceReferences", {
	obj <- OntologySourceReferences$new()
	ex <- BII_I_1_jsonlite$ontologySourceReferences
	obj$from_list(ex)
	expect_equal(obj$to_list(), ex)
})

# Sources ----
test_that("Sources", {
	obj <- Source$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$materials$sources[[1]]
	warns <- capture_warnings(obj$from_list(ex))

	# expect_equal(obj$to_list(), ex)
	# note nulls dropped so comments not checked in unlist comparison
	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})

# Samples ----
test_that("Samples", {
	obj <- Sample$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})

# Materials ----
test_that("Materials", {
	obj <- Material$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$assays[[1]]$materials$otherMaterials[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	#tm_to_list <- tm$to_list()

	# flatten and sort output rather than reorder based on example
	# as this gets annoying to generalise once nested
	# !! might miss something lost when flattening?
	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})

# StudyFactor ----
test_that("StudyFactor", {
	tsf <- StudyFactor$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$factors[[1]]
	warns <- capture_warnings(tsf$from_list(ex))

	## retain order in source when reading? !!
	tsf_to_list <- tsf$to_list()
	expect_equal(unlist_sort_by_name(tsf_to_list), unlist_sort_by_name(ex))

	# sf_order <- names(ex)
	# tsf_to_list <- tsf_to_list[sf_order]
	# ont_anno_order <- names(ex$factorType)
	# tsf_to_list$factorType <- tsf_to_list$factorType[ont_anno_order]
	#
	# expect_equal(tsf_to_list, ex)
})

# StudyFactorReferences ----
test_that("StudyFactorReferences", {
	tsfr <- StudyFactorReferences$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$factors
	warns <- capture_warnings(tsfr$from_list(ex))

	tsfr_to_list <- tsfr$to_list()

	expect_equal(unlist_sort_by_name(tsfr_to_list), unlist_sort_by_name(ex))

	# handinling the possibility of arbitrary name order in json by re-ordering
	# the to_list output to match that of the json as read
	# nms_order <- ex %>% purrr::map(names)
	# tsfr_to_list <- purrr::map2(tsfr_to_list, nms_order, ~.x[.y])
	#
	# lst_lgl <- purrr::map(ex, ~purrr::map_lgl(.x , is.list))
	#
	# nms_order_ft <- purrr::map2(ex, lst_lgl, ~names(.x[.y]$factorType))
	# tsfr_to_list <- purrr::map2(tsfr_to_list, nms_order_ft, ~{
	# 	.x$factorType <- .x$factorType[.y]
	# 	.x
	# })
	#
	# expect_equal(tsfr_to_list, BII_I_1_jsonlite$studies[[1]]$factors)
})

# Characteristic Categories References----
test_that("CharacteristicCategoriesReferences", {
	tccr <- CharacteristicCategoryReferences$new()
	ex <- BII_I_1_jsonlite$studies[[1]]$characteristicCategories
	warns <- capture_warnings(tccr$from_list(ex))
	expect_equal(tccr$to_list(), ex)
})

# Unit Categories ----
test_that("Unit References work", {
	obj <- UnitReferences$new()
	ex <- BII_I_1_jsonlite[["studies"]][[1]]$unitCategories
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	expect_equal(obj$to_list(), ex)
})

# Protocol ----
test_that("Protocol", {
	# protocolType has some unusual behaviour, only annotation value is
	# explicitly listed in the json example, with source and accession left out
	obj <- Protocol$new()
	ex <- BII_I_1_jsonlite[["studies"]][[1]][["protocols"]][[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_equal(obj$to_list(), ex)
})

# Process ----
test_that("Process Sequence",{
	obj <- Process$new()
	ex <- BII_I_1_jsonlite[["studies"]][[1]][["processSequence"]][[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_equal(obj$to_list(), ex)
})

# Publication ----
test_that("Publication", {
	obj <- Publication$new()
	ex <- BII_I_1_jsonlite$publications[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	ont_anno_order <- names(ex$status)
	obj_to_list <- obj$to_list()
	obj_to_list$status <- obj_to_list$status[ont_anno_order]
	expect_equal(obj_to_list, ex)
})

# Person ----
test_that("Person", {
	obj <- Person$new()
	ex <- BII_I_1_jsonlite$people[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Empty email", all = FALSE)

	ont_anno_order <- names(ex$roles[[1]])
	obj_to_list <- obj$to_list()
	obj_to_list$roles <- purrr::map(obj_to_list$roles, ~.x[ont_anno_order])

	expect_equal(tp_to_list, ex)
})

# Assay ----
test_that("Assay", {
	obj <- Assay$new()
	ex <- BII_S_3_jsonlite$studies[[1]]$assays[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_equal(obj$to_list(), ex)

	# expect_equal(ts$submission_date, "2008-08-15")
	# expect_equal(ts$public_release_date, "2008-08-15")

	# ts$factors
})

# Study ----
test_that("Study", {
	ts <- Study$new()
	ts$from_list(BII_S_3_jsonlite$studies[[1]])
	expect_equal(BII_S_3_jsonlite$studies[[1]], ts$to_list())

	# expect_equal(ts$submission_date, "2008-08-15")
	# expect_equal(ts$public_release_date, "2008-08-15")

	# ts$factors
})

# Investigation ----

test_that("Investigation Works", {
	inv <- Investigation$new()
	inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)
	expect_equal(BII_I_1_jsonlite, inv$to_list())

	# submission date
	# description
	# identifier
	# title
	# comments
})

