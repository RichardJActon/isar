# Testing Against Example json Files ----
#
# High level testing to see that objects can be successfully built from and then
# re-serialised to json inputs.
#
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

# library(devtools)
# load_all()

# inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)

json_example <- BII_I_1_jsonlite

## OntologyAnnotation ----

#json read/write

test_that("OntologyAnnotation", {
	obj <- OntologyAnnotation$new()
	ex <- json_example$publications[[1]]$status
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	expect_match(warns, "Missing term accession", all = FALSE)

	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})


## OntologySource ----
test_that("OntologySource  json read/write", {
	ontology_sourcess <- c(
		BII_S_3_jsonlite$ontologySourceReferences,
		json_example$ontologySourceReferences
	)
	for (ex in ontology_sourcess) {
		obj <- OntologySource$new()
		# ex <- json_example$ontologySourceReferences[[1]]
		obj$from_list(ex)
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## OntologySourceReferences ----
test_that("OntologySourceReferences json read/write", {
	ontology_source_references <- list(
		BII_S_3_jsonlite$ontologySourceReferences,
		json_example$ontologySourceReferences
	)
	for (ex in ontology_source_references) {
		obj <- OntologySourceReferences$new()
		# ex <- json_example$ontologySourceReferences
		obj$from_list(ex)
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Sources ----
test_that("Sources json read/write", {
	sources <- c(
		json_example$studies[[1]]$materials$sources,
		json_example$studies[[2]]$materials$sources,
		BII_S_3_jsonlite$studies[[1]]$materials$sources
	)
	for (ex in sources) {
		obj <- Source$new()
		# ex <- json_example$studies[[1]]$materials$sources[[1]]
		warns <- capture_warnings(obj$from_list(ex))

		# expect_equal(obj$to_list(), ex)
		# note nulls dropped so comments not checked in unlist comparison
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Samples ----
test_that("Samples json read/write", {
	samples <- c(
		json_example$studies[[1]]$materials$samples,
		json_example$studies[[2]]$materials$samples,
		BII_S_3_jsonlite$studies[[1]]$materials$samples
	)
	for (ex in samples) {
		obj <- Sample$new()
		# ex <- json_example$studies[[1]]$materials$samples[[1]]
		warns <- capture_warnings(obj$from_list(ex))
		# fix unit from_list for other conditions
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Materials ----
test_that("Materials json read/write", {
	get_materials <- function(invs) {
		invs %>% purrr::map(~{
			.x$studies %>% purrr::map(~{
				.x$assays %>% purrr::map(~{
					.x$materials$otherMaterials
				}) %>% unlist(recursive = FALSE)
			}) %>% unlist(recursive = FALSE)
		}) %>% unlist(recursive = FALSE)
	}
	materials <- get_materials(list(json_example, BII_S_3_jsonlite))
	for (ex in materials) {
		obj <- Material$new()
		#ex <- json_example$studies[[1]]$assays[[1]]$materials$otherMaterials[[1]]
		warns <- capture_warnings(obj$from_list(ex))
		#tm_to_list <- tm$to_list()

		# flatten and sort output rather than reorder based on example
		# !! might miss something lost when flattening!
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## StudyFactor ----
test_that("StudyFactor json read/write", {
	factors <- c(
		json_example$studies[[1]]$factors,
		json_example$studies[[2]]$factors,
		BII_S_3_jsonlite$studies[[1]]$factors
	)
	for (ex in factors) {
		obj <- StudyFactor$new()
		# ex <- json_example$studies[[1]]$factors[[1]]
		warns <- capture_warnings(obj$from_list(ex))

		## retain order in source when reading? !!
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}

	# sf_order <- names(ex)
	# tsf_to_list <- tsf_to_list[sf_order]
	# ont_anno_order <- names(ex$factorType)
	# tsf_to_list$factorType <- tsf_to_list$factorType[ont_anno_order]
	#
	# expect_equal(tsf_to_list, ex)
})

## StudyFactorReferences ----
test_that("StudyFactorReferences json read/write", {
	factor_references <- list(
		json_example$studies[[1]]$factors,
		json_example$studies[[2]]$factors,
		BII_S_3_jsonlite$studies[[1]]$factors
	)
	for (ex in factor_references) {
		obj <- StudyFactorReferences$new()
		# ex <- json_example$studies[[1]]$factors
		warns <- capture_warnings(obj$from_list(ex))

		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}

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
	# expect_equal(tsfr_to_list, json_example$studies[[1]]$factors)
})

## Characteristic Categories References----
test_that("CharacteristicCategoriesReferences json read/write", {
	characteristic_categories_references <- list(
		json_example$studies[[1]]$characteristicCategories,
		json_example$studies[[2]]$characteristicCategories,
		BII_S_3_jsonlite$studies[[1]]$characteristicCategories
	)
	for (ex in characteristic_categories_references) {
		obj <- CharacteristicCategoryReferences$new()
		# ex <- json_example$studies[[1]]$characteristicCategories
		warns <- capture_warnings(obj$from_list(ex))
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Unit Categories ----
test_that("UnitReferences json read/write", {
	unit_references <- list(
		json_example$studies[[1]]$unitCategories,
		# json_example$studies[[2]]$unitCategories, empty
		BII_S_3_jsonlite$studies[[1]]$unitCategories
	)
	for (ex in unit_references) {
		obj <- UnitReferences$new()
		# ex <- json_example[["studies"]][[1]]$unitCategories
		warns <- capture_warnings(obj$from_list(ex))
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

test_that("UnitReferences json read example", {
	obj <- UnitReferences$new()
	ex <- json_example[["studies"]][[1]]$unitCategories
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Term Source Unknown", all = FALSE)
	expect_match(warns, "Term not in source", all = FALSE)
	expect_equal(unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex))
})

## Protocol ----
test_that("Protocol json read/write", {
	protocols  <- c(
		json_example$studies[[1]]$protocols,
		json_example$studies[[2]]$protocols,
		BII_S_3_jsonlite$studies[[1]]$protocols
	)
	# protocolType has some unusual behaviour, only annotation value is
	# explicitly listed in the json example, with source and accession left out
	# in some cases - this is now handled to reflect what is present in the input
	for (ex in protocols) {
		obj <- Protocol$new()
		# ex <- json_example[["studies"]][[1]][["protocols"]][[1]]
		warns <- capture_warnings(obj$from_list(ex))
		# ex$protocolType[["termAccession"]] <- ""
		# ex$protocolType[["termSource"]] <- ""
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Process ----
test_that("Process Sequence json read/write",{
	process_sequences <- c(
		json_example$studies[[1]]$processSequence,
		json_example$studies[[2]]$processSequence,
		BII_S_3_jsonlite$studies[[1]]$processSequence
	)
	for (ex in process_sequences) {
		obj <- Process$new()
		# ex <- json_example[["studies"]][[1]][["processSequence"]][[1]]
		warns <- capture_warnings(obj$from_list(ex))
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

## Publication ----
test_that("Publication json read/write", {
	publications <- c(
		json_example$publications,
		json_example$publications,
		BII_S_3_jsonlite$publications
	)
	for (ex in publications) {
		obj <- Publication$new()
		# ex <- json_example$publications[[1]]
		warns <- capture_warnings(obj$from_list(ex))
		#ont_anno_order <- names(ex$status)
		obj_to_list <- obj$to_list()
		#obj_to_list$status <- obj_to_list$status[ont_anno_order]
		#expect_equal(obj_to_list, ex)
		expect_equal(unlist_sort_by_name(obj_to_list), unlist_sort_by_name(ex))
	}
})

## Person ----
test_that("Person json read/write", {
	people <- c(
		json_example$people, BII_S_3_jsonlite[["studies"]][[1]][["people"]]
	)
	for (ex in people) {
		obj <- Person$new()
		warns <- capture_warnings(obj$from_list(ex))
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}
})

test_that("Person json read example", {
	obj <- Person$new()
	ex <- json_example$people[[1]]
	warns <- capture_warnings(obj$from_list(ex))
	expect_match(warns, "Empty email", all = FALSE)
	# ont_anno_order <- names(ex$roles[[1]])
	#obj_to_list <- obj$to_list()
	#obj_to_list$roles <- purrr::map(obj_to_list$roles, ~.x[ont_anno_order])
	# expect_equal(tp_to_list, ex)
})

## Assay ----
test_that("Assay json read/write", {
	assays <- c(
		json_example$studies[[1]]$assays,
		json_example$studies[[2]]$assays,
		BII_S_3_jsonlite$studies[[1]]$assays
	)
	for (ex in assays) {
		obj <- Assay$new()
		#ex <- BII_S_3_jsonlite$studies[[1]]$assays[[1]]
		# ex <- json_example$studies[[1]]$assays[[1]]
		warns <- capture_warnings(obj$from_list(ex))
		expect_equal(
			unlist_sort_by_name(obj$to_list()), unlist_sort_by_name(ex)
		)
	}

	# expect_equal(ts$submission_date, "2008-08-15")
	# expect_equal(ts$public_release_date, "2008-08-15")

	# ts$factors
})

## Study ----
test_that("Study json read/write", {
	studies <- c(BII_S_3_jsonlite$studies[1], json_example$studies)
	for (ex in studies) {
		obj <- Study$new()
		warns <- capture_warnings(obj$from_list(ex))
		obj_lst <- obj$to_list()

		expect_equal(obj_lst$filename, ex$filename)
		expect_equal(obj_lst$identifier, ex$identifier)
		expect_equal(obj_lst$`@id`, ex$`@id`)
		expect_equal(obj_lst$title, ex$title)
		expect_equal(obj_lst$description, ex$description)
		expect_equal(obj_lst$submissionDate, ex$submissionDate)
		expect_equal(obj_lst$publicReleaseDate, ex$publicReleaseDate)
		purrr::walk2(obj_lst$studyDesignDescriptors, ex$studyDesignDescriptors,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$publications, ex$publications,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$people, ex$people,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$factors, ex$factors,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$protocols, ex$protocols,
			 ~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$assays, ex$assays,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$materials$sources, ex$materials$sources,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		##
		# issue with the value not becoming the annotation value when ontology annotation used
		# need to add terms
		# non numeric entries: obj_lst$materials$sources[[4]]$characteristics %>% purrr::map(~.x$value)
		# e.g. number 28
		#
		# ex[["materials"]][["sources"]][[1]][["characteristics"]][[28]]
		# obj$characteristic_categories$categories[["#characteristic_category/fluorescence"]]$ontology_source_references
		purrr::walk2(obj_lst$materials$samples, ex$materials$samples,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$materials$otherMaterials, ex$materials$otherMaterials,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$characteristicCategories, ex$characteristicCategories,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$processSequence, ex$processSequence,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$comments, ex$comments,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)
		purrr::walk2(obj_lst$unitCategories, ex$unitCategories,
			~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
		)

		# Whole Object
		expect_equal(unlist_sort_by_name(obj_lst), unlist_sort_by_name(ex))
	}
})

## Investigation ----

test_that("Investigation json read/write", {
	obj <- Investigation$new()
	ex <- json_example
	warns <- capture_warnings(obj$from_list(ex, recursive = TRUE, json = TRUE))
	obj_lst <- obj$to_list()

	expect_equal(obj_lst$filename, ex$filename)
	expect_equal(obj_lst$identifier, ex$identifier)
	expect_equal(obj_lst$title, ex$title)
	expect_equal(obj_lst$description, ex$description)
	expect_equal(obj_lst$submissionDate, ex$submissionDate)
	expect_equal(obj_lst$publicReleaseDate, ex$publicReleaseDate)
	purrr::walk2(obj_lst$publications, ex$publications,
		~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
	)
	purrr::walk2(obj_lst$people, ex$people,
		~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
	)
	purrr::walk2(obj_lst$studies, ex$studies,
		~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
	)
	purrr::walk2(obj_lst$comments, ex$comments,
		~expect_equal(unlist_sort_by_name(.x), unlist_sort_by_name(.y))
	)

	# Whole Object
	expect_equal(unlist_sort_by_name(obj_lst), unlist_sort_by_name(ex))
})

