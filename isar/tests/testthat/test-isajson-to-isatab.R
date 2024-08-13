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
test_that("* s_BII-S-1.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()

	obj$studies[["BII-S-1"]]$cat_table(test_output, overwrite = TRUE)

	# stab <- readr::read_tsv(test_file_paths[["s_BII-S-1"]], name_repair = "minimal")
	# ttab <- readr::read_tsv(test_output, name_repair = "minimal")

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

test_that("a_metabolome.txt can be generated from BII-I-1.json", {
	test_output <- tempfile()
	obj$studies[["BII-S-1"]]$assays[["#assay/a_metabolome.txt"]]$cat_table(
		test_output, overwrite = TRUE
	)
	#obj$studies[["BII-S-2"]]$to_table()

	# stab <- readr::read_tsv(
	# 	test_file_paths[["a_metabolome"]],
	# 	name_repair = "unique_quiet", show_col_types = FALSE
	# )
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

