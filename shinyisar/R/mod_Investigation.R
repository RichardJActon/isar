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
  tagList(
 
  )
}
    
#' Investigation Server Functions
#'
#' @noRd 
mod_Investigation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Investigation_ui("Investigation_1")
    
## To be copied in the server
# mod_Investigation_server("Investigation_1")
