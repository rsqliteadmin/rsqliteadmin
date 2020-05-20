#' side_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_side_panel_ui <- function(id) {
  ns <- NS(id)
  selectInput(ns("active_db"), "Select a database to work on",
              choices = db_list())
}

#' side_panel Server Function
#'
#' @noRd
mod_side_panel_server <-
  function(input, output, session, button_clicked) {
    ns <- session$ns
    observeEvent(button_clicked(), {
      updateSelectInput(session,
                        inputId =  "active_db",
                        choices = db_list())
    })
  }

## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")

## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")
