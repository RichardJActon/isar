#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny markdown
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar dashboardControlbar dashboardFooter dashboardBody
#' @noRd
app_ui <- function(request) {
	tagList(
		# Leave this function for adding external resources
		golem_add_external_resources(),
		# Your application UI logic
		bs4Dash::dashboardPage(
			title = "Shiny ISA R",
			header = bs4Dash::dashboardHeader(),
			sidebar = bs4Dash::dashboardSidebar(
				mod_Investigation_sidebar_ui("Investigation_1")
			),
			controlbar = bs4Dash::dashboardControlbar(),
			footer = bs4Dash::dashboardFooter(
				left = shiny::markdown("&copy; 2023 [HDBI](https://hdbi.org/). License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)"),
				right = shiny::img(
					alt = "Wellcome Trust",
					src = "inst/app/img/Wellcome_Trust_logo.svg",
					href = "https://wellcome.org/",
					width = 32
				)
			),
			body = bs4Dash::dashboardBody(
				bs4Dash::tabItems(
					mod_Investigation_body_ui("Investigation_1")
				)
			)
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
