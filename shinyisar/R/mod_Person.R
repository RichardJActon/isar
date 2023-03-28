#' Person UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Person_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Person Server Functions
#'
#' @noRd 
mod_Person_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Person_ui("Person_1")
    
## To be copied in the server
# mod_Person_server("Person_1")
