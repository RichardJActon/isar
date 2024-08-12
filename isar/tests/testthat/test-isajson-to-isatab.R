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
test_that("s_BII-S-1.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()

	obj$studies[["BII-S-1"]]$cat_table(test_output, overwrite = TRUE)

	stab <- readr::read_tsv(test_file_paths[["s_BII-S-1"]], name_repair = "minimal")
	ttab <- readr::read_tsv(test_output, name_repair = "minimal")

	expect_equal(
		stab[,order(colnames(stab))] %>% dplyr::arrange(dplyr::everything()),
		ttab[,order(colnames(ttab))] %>% dplyr::arrange(dplyr::everything())
	)

	# slines <- readLines(test_file_paths[["s_BII-S-1"]], n = 1)
	# tlines <- readLines(test_output, n = 1)
	#
	# scolnms <- slines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	# tcolnms <- tlines[[1]] %>% strsplit(split = "\t") %>% `[[`(1)
	#
	# expect_equal(sort(scolnms), sort(tcolnms))

	fs::file_delete(test_output)
})
