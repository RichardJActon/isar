#' Assay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Assay_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Assay Server Functions
#'
#' @noRd 
mod_Assay_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Assay_ui("Assay_1")
    
## To be copied in the server
# mod_Assay_server("Assay_1")
