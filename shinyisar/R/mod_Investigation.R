#' Investigation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Investigation_ui <- function(id){
	ns <- NS(id)
	sidebarMenu(
		id = ns("sidebarmenu"),
		sidebarHeader("inv title"),
	)

}

#' Investigation sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_Investigation_sidebar_ui <- function(id){
	ns <- NS(id)
	shiny::uiOutput(ns("sidebar_menu"))
}

#' Investigation Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent renderUI
#' @importFrom gargoyle watch
#' @importFrom bs4Dash sidebarMenu sidebarHeader
mod_Investigation_sidebar_server <- function(id, investigation){
	moduleServer( id, function(input, output, session){
		ns <- session$ns
		shiny::observeEvent(
			gargoyle::watch("update_investigation"), {
				output$sidebar_menu <- shiny::renderUI({
					lst_of_tabs <- switch(
						ifelse(is.null(investigation$studies),"1","2"),
						"1" = NULL, "2" = {
							purrr::map(investigation$studies, ~{
								output[[paste0("tab_", .x$get_id())]] <- shiny::renderUI(
									bs4Dash::bs4SidebarMenuItem(
										text = .x$title,
										tabName = paste0("tab_", .x$get_id())
									)
								)
								shiny::uiOutput(ns(paste0("tab_", .x$get_id())))
							})
						}
					)
					bs4Dash::sidebarMenu(
						id = ns("sidebar_menu"),
						bs4Dash::sidebarHeader("Investigation"),
						bs4Dash::bs4SidebarMenuItem(
							text = investigation$title,
							tabName = ns(paste0("tab_", investigation$get_id())),
							.list = lst_of_tabs
						)
					)
				})
			}
		)
	})
}


#' Investigation sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_Investigation_body_ui <- function(id){
	ns <- NS(id)
	shiny::uiOutput(ns("body"))
}

#' Investigation Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent renderUI
#' @importFrom gargoyle watch
#' @importFrom bs4Dash sidebarMenu sidebarHeader
mod_Investigation_body_server <- function(id, investigation){
	moduleServer(id, function(input, output, session){
		ns <- session$ns
		shiny::observeEvent(
			gargoyle::watch("update_investigation"), {
				output$body <- shiny::renderUI(
					bs4Dash::tabItem(
						tabName = paste0("tab_",investigation$get_id()),
						shiny::fluidRow(
							bs4Dash::box(
								title = shiny::h1(investigation$title),
								investigation$description
							)
						)
					)
				)
			}
		)
	})
}

## To be copied in the UI
# mod_Investigation_ui("Investigation_1")

## To be copied in the server
# mod_Investigation_server("Investigation_1")
