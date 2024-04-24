# BII_I_1_tab <- readr::read_tsv(
# 	fs::path_abs(fs::path_join(c(
# 		testthat::test_path(), "test-data/ISAdatasets/tab/BII-I-1/i_investigation.txt"
# 	)))
# )
BII_S_3_tab <- readr::read_tsv(
	fs::path_abs(fs::path_join(c(
		testthat::test_path(), "test-data/ISAdatasets/tab/BII-S-3/s_BII-S-3.txt"
	)))

)
BII_S_3_assay_Gx_tab <- readr::read_tsv(
	fs::path_abs(fs::path_join(c(
		testthat::test_path(), "test-data/ISAdatasets/tab/BII-S-3/a_gilbert-assay-Gx.txt"
	)))

)
# ISA-tab directory validation works ----
test_that("ISA-tab directory validation works", {
	## Directory exists ----
	isa_tab_example_dir <- paste0("/not/a/directory", uuid::UUIDgenerate())
	expect_error(
		isa_tab_example_dir %>%
			check_isa_tab_dir() %>%
			error_with_check_message_on_failure()
	)
	## There are files in the directory ----
	isa_tab_example_dir <- fs::path_temp()
	purrr::walk(fs::dir_ls(isa_tab_example_dir), fs::file_delete)
	expect_error(
		isa_tab_example_dir %>%
			check_isa_tab_dir() %>%
			error_with_check_message_on_failure(),
		regexp = paste0(
			"No files found in directory: '", isa_tab_example_dir, "'!"
		)
	)
	## ISA-tab files have the right naming ----
	temp_files <- c(
		fs::file_temp(pattern = "i_investigation", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "s_study", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "s_study", tmp_dir = isa_tab_example_dir, ext = "csv"),
		fs::file_temp(pattern = "a_assay", tmp_dir = isa_tab_example_dir, ext = "txt")
	)
	purrr::walk(temp_files, fs::file_touch)

	## ISA-tab files have the right file extension ----
	expect_error(
		isa_tab_example_dir %>%
			check_isa_tab_dir() %>%
			error_with_check_message_on_failure(),
		regexp = "ISA-tab files must be prefixed i\\/s\\/a_ and have the extension \\.txt\\!"
	)
	purrr::walk(fs::dir_ls(isa_tab_example_dir), fs::file_delete)
	## Only 1 investigation per directory ----
	temp_files <- c(
		fs::file_temp(pattern = "i_investigation", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "i_investigation2", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "s_study", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "a_assay", tmp_dir = isa_tab_example_dir, ext = "txt")
	)
	purrr::walk(temp_files, fs::file_touch)

	expect_error(
		isa_tab_example_dir %>%
			check_isa_tab_dir() %>%
			error_with_check_message_on_failure(),
		regexp = "More than one Investigation file found\\!"
	)

	purrr::walk(fs::dir_ls(isa_tab_example_dir), fs::file_delete)

	## At leasts of Study and Assay files ----
	temp_files <- c(
		fs::file_temp(pattern = "i_investigation", tmp_dir = isa_tab_example_dir, ext = "txt"),
		#fs::file_temp(pattern = "s_study", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "a_assay", tmp_dir = isa_tab_example_dir, ext = "txt")
	)
	purrr::walk(temp_files, fs::file_touch)

	expect_error(
		isa_tab_example_dir %>%
			check_isa_tab_dir() %>%
			error_with_check_message_on_failure(),
		regexp = "There must be at least 1 investigation, 1 study, and 1 assay"
	)

	purrr::walk(fs::dir_ls(isa_tab_example_dir), fs::file_delete)
})

test_that("get_isatab_files works", {
	isa_tab_example_dir <- fs::path_temp()

	temp_files <- c(
		fs::file_temp(pattern = "i_investigation", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "s_study", tmp_dir = isa_tab_example_dir, ext = "txt"),
		fs::file_temp(pattern = "a_assay", tmp_dir = isa_tab_example_dir, ext = "txt")
	)
	purrr::walk(temp_files, fs::file_touch)

	expect_message(
		files_tab <- get_isatab_files(isa_tab_example_dir, verbose = TRUE),
		regexp = "1 Investigation"
	)
	expect_true(all(temp_files %in% files_tab$paths))
	expect_true(all(fs::path_file(temp_files) %in% files_tab$files))
	expect_equal(
		factor(c("i","s","a"), levels = c("i","s","a"), ordered = TRUE),
		files_tab$type
	)

	purrr::walk(temp_files, fs::file_delete)

	# Need more negative test cases
})
