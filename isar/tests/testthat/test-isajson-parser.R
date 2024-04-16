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
inv <- Investigation$new()
# inv$from_list(BII_I_1_jsonlite, recursive = FALSE, json = TRUE)
inv$from_list(BII_I_1_jsonlite, recursive = TRUE, json = TRUE)
# inv$get_ontology_source_names()
# detecting missing ontology sources
## Roles

# BII_I_1_jsonlite[["people"]][[1]][["roles"]]
# BII_I_1_jsonlite[["studies"]][[1]][["people"]][[1]][["roles"]]

# Person ----
test_that("Person", {
	p1 <- Person$new(`@id` = test_uuid)
	expect_warning(
		p1$from_list(BII_I_1_jsonlite$people[[1]]),#, json = TRUE
		"Empty email"
	)
	p1 <- Person$new()
	p1$set_id(test_uuid)
	p1$from_list(BII_S_3_jsonlite[["studies"]][[1]][["people"]][[1]])
})


