library(shiny)

exr6 <- R6::R6Class(
	"exr6",
	public = list(
		example = NULL,
		initialize = function(example) {
			self$example <- example
		},
		get_id = function() {
			private$id
		}
	),
	private = list(
		id = uuid::UUIDgenerate()
	)
)

ui <- fluidPage(
	shiny::uiOutput("ui")
)

server <- function(input, output, session) {
	exr6i <- exr6$new(
		example = list(
			list(title = "test", id = 1), list(title = "b", id = 2)
		)
	)
	gargoyle::init("update_investigation")

	output$ui <- shiny::renderUI({
		lst_of_tabs <- switch(
			ifelse(is.null(exr6i$example),"1","2"),
			"1" = NULL, "2" = {
				purrr::map(exr6i$example, ~{
					output[[paste0("tab_", .x$id)]] <- shiny::renderUI(
						bs4Dash::bs4SidebarMenuItem(
							text = .x$title,
							tabName = paste0("tab_", .x$id)
						)
					)
					shiny::uiOutput(paste0("tab_", .x$id))
				})
			}
		)
		bs4Dash::sidebarMenu(
			id = "sidebar_menu",
			bs4Dash::sidebarHeader("Investigation"),
			bs4Dash::bs4SidebarMenuItem(
				text = exr6i$title,
				tabName = paste0("tab_", exr6i$get_id()),
				.list = lst_of_tabs
			)
		)
	})
}

shinyApp(ui, server)
