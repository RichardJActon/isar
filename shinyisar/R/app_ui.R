#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
	tagList(
		# Leave this function for adding external resources
		golem_add_external_resources(),
		# Your application UI logic
		bs4Dash::dashboardPage(
			title = "Shiny ISA R",
			header = dashboardHeader(),
			sidebar = dashboardSidebar(),
			controlbar = dashboardControlbar(),
			footer = dashboardFooter(
				left ="&copy; 2023 [HDBI](https://hdbi.org/). License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)",
				right ="[![Wellcome Trust](/img/wellcome-trust-logo.svg){width=32px}](https://wellcome.org/)",
			),
			body = dashboardBody()
		)
	)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyisar"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
