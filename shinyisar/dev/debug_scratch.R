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

ui <- bs4Dash::dashboardPage(
	title = "Shiny ISA R",
	header = bs4Dash::dashboardHeader(),
	sidebar = bs4Dash::dashboardSidebar(
		bs4Dash::sidebarMenu(
			id = "sidebar_menu",
			bs4Dash::sidebarHeader("Investigation"),
			bs4Dash::bs4SidebarMenuItem(
				text = "demo",
				tabName = "demo",
				.list = list(
					bs4Dash::bs4SidebarMenuItem(
						text = "study_1",
						tabName = "study_1"
					),
					bs4Dash::bs4SidebarMenuItem(
						text = "study_2",
						tabName = "study_2"
					)
				)
			)
		)
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
			bs4Dash::tabItem(
				tabName = "demo",
				shiny::fluidRow(
					bs4Dash::box(
						title = shiny::h1("demo"),
						"some text..."
					)
				)
			),
			list(
				bs4Dash::tabItem(
					tabName = "study_1",
					shiny::fluidRow(
						bs4Dash::box(
							title = shiny::h1("study_1"),
							"some text..."
						)
					)
				),
				bs4Dash::tabItem(
					tabName = "study_2",
					shiny::fluidRow(
						bs4Dash::box(
							title = shiny::h1("study_2"),
							"some text..."
						)
					)
				)
			)
		)
	)
)


server <- function(input, output, session) {
	exr6i <- exr6$new(
		example = list(
			list(title = "test", id = 1), list(title = "b", id = 2)
		)
	)
	gargoyle::init("update_investigation")
	shiny::observeEvent(
		gargoyle::watch("update_investigation"), {
			output$ui <- shiny::renderUI({
				lst_of_tabs <- switch(
					ifelse(is.null(exr6i$example),"1","2"),
					"1" = NULL, "2" = {
						purrr::map(exr6i$example, ~{
							output[[paste0("tab_", .x$id)]] <- shiny::renderUI({
								shiny::tagList(
									shiny::textInput(
										inputId = .x$title,
										label = paste0("tab_", .x$id)
									#bs4Dash::bs4SidebarMenuItem(
										# text = .x$title,
										# tabName = paste0("tab_", .x$id)
									),
									shiny::textOutput(.x$title,inline = TRUE)
								)
							})
							shiny::uiOutput(paste0("tab_", .x$id))
						})
					}
				)
				shiny::fluidRow(
					shiny::h2(exr6i$get_id()),
					lst_of_tabs
				)
				# bs4Dash::sidebarMenu(
				# 	id = "sidebar_menu",
				# 	bs4Dash::sidebarHeader("Investigation"),
				# 	bs4Dash::bs4SidebarMenuItem(
				# 		text = exr6i$title,
				# 		tabName = paste0("tab_", exr6i$get_id()),
				# 		.list = lst_of_tabs
				# 	)
				# )
			})
		}
	)

}


server <- function(input, output, session) {

}
shinyApp(ui, server)
