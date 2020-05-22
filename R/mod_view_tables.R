#' view_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_view_tables_ui <- function(id) {
  ns <- NS(id)
  tabPanel(title = "View/Edit Tables",
           br(),
           fluidRow(selectInput(
             ns("select_table"), "Select a table to view", choices = NULL
           )))
}

#' view_tables Server Function
#'
#' @noRd
mod_view_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  observe({
    updateSelectInput(session,
                      inputId = "select_table",
                      choices = RSQLite::dbListTables(conn$active))
  })
  
}

## To be copied in the UI
# mod_view_tables_ui("view_tables_ui_1")

## To be copied in the server
# callModule(mod_view_tables_server, "view_tables_ui_1")
