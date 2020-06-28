#' query UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_query_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Query",
    br(),
    fluidRow(shinyjqui::jqui_resizable(
      shinyAce::aceEditor(
        outputId = ns("ace"),
        placeholder = "Enter query here.",
        mode = "sql",
        height = "200px"
      )
    )),
    fluidRow(
      actionButton(inputId = ns("execute"),
                   label = "Execute Query"),
      br(),
      br(),
      verbatimTextOutput(ns("display_error"))
    ),
    br(),
    fluidRow(uiOutput(ns(
      "query_results_ui"
    )))
  )
}

#' query Server Function
#'
#' @noRd
mod_query_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(data = NULL,
                         error = NULL)
  
  action_query <- reactiveValues(data_updated = NULL)
  
  output$display_error <- renderText({
    info$error
  })
  
  observeEvent(input$execute, {
    if (!is.null(conn$active_db)) {
      query <- input$ace
      query <- gsub("\n", " ", query)
      tryCatch({
        if (isTRUE(grepl("select", query, ignore.case = TRUE))) {
          info$data <- RSQLite::dbGetQuery(conn$active_db, query)
          showNotification(ui = "Query Completed.",
                           duration = 5,
                           type = "message")
        }
        else{
          RSQLite::dbExecute(conn$active_db, query)
          action_query$data_updated <- input$execute
          showNotification(ui = "Query Completed.",
                           duration = 3,
                           type = "message")
        }
      },
      error = function(err) {
        info$error <- toString(err)
      })
    }
    else{
      showNotification(ui = "No database selected.",
                       duration = 3,
                       type = "error")
    }
  })
  
  output$query_results_ui <- renderUI({
    column(
      width = 12,
      style = "height:500px;overflow-y: scroll;overflow-x: scroll;",
      conditionalPanel(condition = !is.null(info$data),
                       fluidRow(DT::DTOutput(
                         ns("query_results")
                       )))
    )
  })
  
  output$query_results <-
    DT::renderDT(expr = {
      DT::datatable(data = info$data)
    })
  
  return(action_query)
  
}

## To be copied in the UI
# mod_query_ui("query_ui_1")

## To be copied in the server
# callModule(mod_query_server, "query_ui_1")
