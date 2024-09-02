# Can isar generate:
# - i_investigation.txt
# - a_metabolome.txt
# - a_microarray.txt
# - a_transcriptome.txt
# - s_BII-S-1.txt
# - s_BII-S-2.txt
# from BII-I-1.json
BII_I_1_jsonlite <- jsonlite::read_json(
	fs::path_abs(fs::path_join(c(
		testthat::test_path(), "test-data/ISAdatasets/json/BII-I-1/BII-I-1.json"
	)))
)

test_files <- c(
	"i_investigation.txt",
	"a_metabolome.txt",
	"a_microarray.txt",
	"a_transcriptome.txt",
	"a_proteome.txt",
	"s_BII-S-1.txt",
	"s_BII-S-2.txt"
)
test_file_paths <- test_files %>%
	set_names(sub("\\.txt", "", test_files)) %>%
	purrr::map_chr(~fs::path_abs(fs::path_join(c(
		testthat::test_path(),
		"test-data/ISAdatasets/tab/BII-I-1/",
		.x
	))))

obj <- Investigation$new()
warns <- capture_warnings(obj$from_list(
	BII_I_1_jsonlite, recursive = TRUE, json = TRUE
))

# i_investigation.txt ----
test_that("i_investigation.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()
	obj$cat_table(test_output, overwrite = TRUE)

	ilines <- readLines(test_file_paths[["i_investigation"]])
	tlines <- readLines(test_output)

	# comment order is not preserved so excluding them from the comparison
	ilinesNC <- ilines[!grepl("^Comment\\[", ilines)]
	tlinesNC <- tlines[!grepl("^Comment\\[", tlines)]
	expect_equal(ilinesNC, tlinesNC)

	# Checking comments are the same once in the same order
	ilinesC <- sort(ilines[grepl("^Comment\\[", ilines)])
	tlinesC <- sort(tlines[grepl("^Comment\\[", tlines)])

	expect_equal(ilinesC, tlinesC)

	# expect_equal(ilines, tlines)
	fs::file_delete(test_output)
})


# s_BII-S-1.txt ----
test_that("* s_BII-S-1.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()

	obj$studies[["BII-S-1"]]$cat_table(test_output, overwrite = TRUE)

	# stab <- readr::read_tsv(test_file_paths[["s_BII-S-1"]], name_repair = "minimal")
	# ttab <- readr::read_tsv(test_output, name_repair = "minimal")

	# extract name dupe - join?
	# material type
	# MS assay name

	stab <- readr::read_tsv(
		test_file_paths[["s_BII-S-1"]],
		name_repair = "unique_quiet", show_col_types = FALSE
	)
	ttab <- readr::read_tsv(
		test_output,
		name_repair = "unique_quiet", show_col_types = FALSE
	)

	# return to table content testing when table parsing is further along?
	# no simple to compare all of the contents with the ambiguously named
	# columns this is definitely a defect in the standard IMO non-unique column
	# names what where they thinking?

	# expect_equal(
	# 	stab[order(as.data.frame(stab)[,1]),],
	# 	ttab[order(as.data.frame(ttab)[,1]),]
	# 	#
	# 	# stab[order(as.data.frame(stab)[,1]), order(colnames(stab))],
	# 	# ttab[order(as.data.frame(ttab)[,1]), order(colnames(ttab))]
	#
	# 	# stab[order(as.data.frame(stab)[,1]), order(unlist(stab[1,]))],
	# 	# ttab[order(as.data.frame(ttab)[,1]), order(unlist(ttab[1,]))]
	#
	# 	# unlist_sort_by_name(stab), unlist_sort_by_name(ttab)
	# )

	# colnames
	slines <- readLines(test_file_paths[["s_BII-S-1"]], n = 1)
	tlines <- readLines(test_output, n = 1)

	scolnms <- slines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	tcolnms <- tlines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)

	expect_equal(sort(scolnms), sort(tcolnms))

	fs::file_delete(test_output)
})


test_that("BII-S-2.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()
	obj$studies[["BII-S-2"]]$cat_table(test_output, overwrite = TRUE)
	#obj$studies[["BII-S-2"]]$to_table()

	stab <- readr::read_tsv(
		test_file_paths[["s_BII-S-2"]],
		name_repair = "unique_quiet", show_col_types = FALSE
	)
	ttab <- readr::read_tsv(
		test_output,
		name_repair = "unique_quiet", show_col_types = FALSE
	)

	# colnames
	slines <- readLines(test_file_paths[["s_BII-S-2"]], n = 1)
	tlines <- readLines(test_output, n = 1)

	scolnms <- slines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	tcolnms <- tlines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)

	expect_equal(sort(scolnms), sort(tcolnms))

	fs::file_delete(test_output)
})

test_that("a_proteome.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()
	obj$studies[["BII-S-1"]]$assays[["#assay/a_metabolome.txt"]]$cat_table(
		test_output, overwrite = TRUE
	)
	#obj$studies[["BII-S-2"]]$to_table()

	stab <- readr::read_tsv(
		test_file_paths[["a_proteome"]],
		name_repair = "unique_quiet", show_col_types = FALSE
	)
	stab <- readr::read_tsv(
		test_file_paths[["a_transcriptome"]],
		name_repair = "unique_quiet", show_col_types = FALSE
	)
	# ttab <- readr::read_tsv(
	# 	test_output,
	# 	name_repair = "unique_quiet", show_col_types = FALSE
	# )
	#
	# # colnames
	# slines <- readLines(test_file_paths[["a_metabolome"]], n = 1)
	# tlines <- readLines(test_output, n = 1)
	#
	# scolnms <- slines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	# tcolnms <- tlines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	#
	# expect_equal(sort(scolnms), sort(tcolnms))

	fs::file_delete(test_output)
})



# These two lines in a_proteome.txt
#
# line 13
# line 18
#
# labeled extract pool 3 (for P-0.2 and S-0.2) includes iTRAQ reagent 114
# that is missing from the json version
#
# note that isa-api has the same issues with comment duplication:
# https://github.com/ISA-tools/isa-api/blob/master/tests/convert/test_json2isatab.py
#
# Remedy? - alter the json to reflect this, would be good to test multiple
# characteristics as that's a more complex case than the single characteristic
# if I remove the extra lines from the table
#
# Sample Factor values even if they contain an explicit null value
# are missing a connection to material characteristics which map to these
# factor value levels so that they can be correctly connected.
# not all combinations of material characteristics and study factors
# necessarily exist a relationship must be specified to generate a correct table

category <- BII_I_1_jsonlite$studies[[1]]$assays[[1]]$materials$otherMaterials[[17]]$characteristics[[1]]
category$value$annotationValue <- "iTRAQ reagent 114"
BII_I_1_jsonlite$studies[[1]]$assays[[1]]$materials$otherMaterials[[17]]$characteristics[[2]] <- category

# Explicitly empty factor Values?
# handling of multiple values for the same factor ...
BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[3]] <-
list(
	category = list(`@id` = "#factor/limiting_nutrient"),
	value = list(
		annotationValue = "",
		termAccession = "",
		termSource = ""
	)
)
BII_I_1_jsonlite$studies[[1]]$materials$samples[[1]]$factorValues[[4]] <-
	list(
		category = list(`@id` = "#factor/rate"),
		value = "",
		unit = list(`@id` = "#Unit/l/hour")
	)

# undefined mechanism for determining Assay, Data Transformation and Normalisation
# it would make sense that these be defined based on the protocol type
# there are 3 processes with undefined protocols in the proteomics example
# You also need somewhere to define which protocol types count as each of these categories
# a more challenging thing to fit into the existing spec
# a reasonable seeming place to do this is in ontology source references
# add a flag to ontology sources indicating if they are considered a transform,normalisation,or assay
# and this can be used to check if a protocol is of a type that gets special column headings
BII_I_1_jsonlite$studies[[1]]$protocols <- c(
	BII_I_1_jsonlite$studies[[1]]$protocols, list(
	list(
		parameters = list(),
		components = list(),
		uri = "",
		description = "",
		version = "",
		`@id` = "#protocol/assay",
		name = "assay",
		protocolType = list(
			annotationValue = "Mass Spec",
			termSource = "OAT"
		)
	),
	list(
		parameters = list(),
		components = list(),
		uri = "",
		description = "",
		version = "",
		`@id` = "#protocol/transformation",
		name = "transformation",
		protocolType = list(
			annotationValue = "transform",
			termSource = "ODT"
		)
	),
	list(
		parameters = list(),
		components = list(),
		uri = "",
		description = "",
		version = "",
		`@id` = "#protocol/normalization",
		name = "normalization",
		protocolType = list(
			annotationValue = "normalize",
			termSource = "ONM"
		)
	))
)
BII_I_1_jsonlite$ontologySourceReferences <- c(
	BII_I_1_jsonlite$ontologySourceReferences,
	list(
		list(
			file = "",
			name = "OAT",
			description = "Ontology of Assay Types",
			version = "0.0.0-9000",
			isaProcessType = "Normalization"
		),
		list(
			file = "",
			name = "ODT",
			description = "Ontology of Data Transformations",
			version = "0.0.0-9000",
			isaProcessType = "Data Transformation"
		),
		list(
			file = "",
			name = "ONM",
			description = "Ontology of Normalisation Methods",
			version = "0.0.0-9000",
			isaProcessType = "Normalization"
		)
	)
)
# list(transformations = list(), normalisations = list(), assays = list())

# Technology type special cases
# these are a problem they do not generalise
# a spec is needed to provide a generalised way of describing constraints
# also tech type should clearly be an ontology annotation

# data file types:
# Image File, Raw Data File, Derived Data File
# appear to have a specific meaning but this is a free text field.

# extract and labeled extract - appear to just specific cases of characteristics
# unclear justification for special treatment.
