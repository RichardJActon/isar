#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom isar Investigation
#' @importFrom gargoyle init
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
	#investigation <- isar::Investigation$new()
	investigation <- isar::Investigation$new(
		title = "placeholder Investigation",
		description = "Investigating Lorem Ipsum",
		submission_date = "2023-03-31",
		public_release_date = "2023-03-31",
		studies = list(
			Study$new(
				title = "Test Study", description = "Studying Lorem Ipsum"
			),
			Study$new(
				title = "Test Study 2", description = "Studying Lorem Ipsum again"
			)
		)
	)
	gargoyle::init("update_investigation")

	mod_Investigation_sidebar_server("Investigation_1", investigation = investigation)
	mod_Investigation_body_server("Investigation_1", investigation = investigation)

}
