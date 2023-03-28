#' Study UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Study_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Study Server Functions
#'
#' @noRd 
mod_Study_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Study_ui("Study_1")
    
## To be copied in the server
# mod_Study_server("Study_1")
