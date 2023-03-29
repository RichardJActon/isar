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
				output$sidebar_menu <- shiny::renderUI(
					bs4Dash::sidebarMenu(
						id = ns("sidebar_menu"),
						bs4Dash::sidebarHeader(investigation$title)
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
