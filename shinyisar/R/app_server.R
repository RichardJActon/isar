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
	investigation <- isar::Investigation$new(title = "placeholder")
	gargoyle::init("update_investigation")

	mod_Investigation_sidebar_server("Investigation_1", investigation = investigation)
}
