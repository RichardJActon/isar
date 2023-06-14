#' comments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comments_ui <- function(id){
	ns <- NS(id)
	uiOutput(ns("comments"))
	bs4Dash::socialBox(
		collapsible = FALSE,
		title = "comments", width = 12,
		comments = shiny::tagList(
			bs4Dash::boxComment(
				image = NULL,
				title = "a title",
				"body of the comment"
			),
			bs4Dash::boxComment(
				image = NULL,
				title = "a title",
				"body of the comment"
			)
		),
		shiny::textInput("comment_content", "Comment")
		shiny::actionButton("add_comment","Add")
	)
}
    
#' comments Server Functions
#'
#' @noRd 
mod_comments_server <- function(id, r6, event){
	moduleServer( id, function(input, output, session){
		ns <- session$ns
		observeEvent(gargoyle::watch(event), {
			output$comments <- renderUI({
				bx <- purrr::imap(r6$comments, ~{
					textInput(inputId = ns(.y), label = .y, value = .x)
				})
				tagList(bx, actionButton(ns("go"),"go")) 
			})
		})
		
		observeEvent(input$go, {
			new_comments <- names(r6$comments) %>% 
				#purrr::map_chr(ns) %>%
				purrr::set_names() %>% 
				purrr::map(~{input[[.x]]})
			r6$set_comments(new_comments)
			gargoyle::trigger(event)
		})
	})
}
    
## To be copied in the UI
# mod_comments_ui("comments_1")
    
## To be copied in the server
# mod_comments_server("comments_1")
