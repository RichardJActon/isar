#' Person UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Person_card_ui <- function(id){
	ns <- NS(id)
	shiny::uiOutput(ns("person_card"))
}

#' Person Server Functions
#'
#' @noRd
mod_Person_card_server <- function(id, person) {
	moduleServer( id, function(input, output, session) {
		ns <- session$ns
		shiny::observeEvent(
			gargoyle::watch("update_person"), {
				output$person_card <- shiny::renderUI(
					bs4Dash::boxProfile(
						src = NULL,
						title = paste(
							person$first_name, person$middle_initial,
							person$last_name
						),
						subtitle = person$orcid,
						shiny::tagList(
							#bordered = TRUE,
							bs4Dash::boxProfileItem(
								title = "email",
								description = person$email
							),
							bs4Dash::boxProfileItem(
								title = "phone",
								description = person$phone
							),
							bs4Dash::boxProfileItem(
								title = "Affiliation(s)",
								description = person$affiliation
							),
							bs4Dash::boxProfileItem(
								title = "address",
								description = person$address
							),
							shiny::fluidRow(
								bs4Dash::appButton(
									#inputId = ns("edit_person"),
									inputId = "edit_person",
									label = "Edit",
									#icon = "fa fa-edit",
									icon = shiny::icon("edit"),
									enable_badge = FALSE,
									badgeColor = NULL,
									badgeLabel = NULL
								)
							)
						)
					)
				)
			}
		)
		# shiny::observeEvent(intput$edit_person,{
		# 	gargoyle::trigger("update_person")
		# })
	})
}

## To be copied in the UI
# mod_Person_ui("Person_1")

## To be copied in the server
# mod_Person_server("Person_1")
#
# app_ui_test <- function(request) {
# 	ui <- bs4Dash::dashboardPage(
# 		title = "Person Box Test",
# 		header = bs4Dash::dashboardHeader(),
# 		sidebar = bs4Dash::dashboardSidebar(),
# 		controlbar = bs4Dash::dashboardControlbar(),
# 		footer = bs4Dash::bs4DashFooter(),
# 		body = bs4Dash::dashboardBody(
# 			bs4Dash::box(
# 				mod_Person_card_ui("jane_doe")
# 			)
# 		)
#
# 	)
# }
#
# app_server_test <- function(input, output, session) {
#
# 	Jane_Doe <- Person$new(
# 		last_name = "Doe", first_name = "Jane", mid_initials = "C.",
# 		phone = "01234567890", fax = "12345",
# 		address = "The Library, Unseen University",
# 		roles = list(Conceptualization),
# 		email = "Jane.Doe@email.com",
# 		affiliation = "Unseen University",
# 		orcid = "0000-8888-8888-8888",
# 		comments = list(
# 			"namespace collision" = "good thing Jane has an ORCID"
# 		)
# 	)
#
# 	gargoyle::init("update_person")
#
# 	mod_Person_card_server("jane_doe", Jane_Doe)
# }
#
# shinyApp(app_ui_test, app_server_test)
