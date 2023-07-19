test_that("ISA-tab directory validation works", {
	isa_tab_example_dir <- tempdir()
	tempfile(pattern = "i_investigation", tmpdir = isa_tab_example_dir, fileext = "txt")
	tempfile(pattern = "i_investigation2", tmpdir = isa_tab_example_dir, fileext = "txt")
	tempfile(pattern = "s_study", tmpdir = isa_tab_example_dir, fileext = "txt")
	tempfile(pattern = "s_study", tmpdir = isa_tab_example_dir, fileext = "csv")
	tempfile(pattern = "a_assay", tmpdir = isa_tab_example_dir, fileext = "txt")
	
	expect_error(isa_tab_dir_validate(isa_tab_example_dir))
})
