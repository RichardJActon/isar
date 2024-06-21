library(targets)
library(tarchetypes)
library(isar)

list(
	tar_target(
		investigation_data,
		c("../isar/tests/testthat/test-data/ISAdatasets/json/BII-I-1/BII-I-1.json"),
		format = "file"
	),
	tar_target(
		investigations, {
			inv <- isar::Investigation$new()
			inv$from_list(investigation_data)
			inv
		},
		pattern = map(investigation_data)
	)# ,
	# tar_quarto_rep(
	# 	investigation_pages,
	# 	"investigation.qmd",
	# 	params = tibble::tibble(
	# 		par = c(),
	# 		output_file = c()
	# 	)
	# )

)
