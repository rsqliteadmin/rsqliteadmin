#' test_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_module_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' test_module Server Function
#'
#' @noRd 
mod_test_module_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_test_module_ui("test_module_ui_1")
    
## To be copied in the server
# callModule(mod_test_module_server, "test_module_ui_1")
 
