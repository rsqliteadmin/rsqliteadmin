#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_search_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "Search",
           br(),)
}

#' search Server Function
#'
#' @noRd

mod_search_server <- function(input, output, session, conn) {
  ns <- session$ns
  
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# callModule(mod_search_server, "search_ui_1")

