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
  tabPanel(
    title = "Search",
    br(),
    fluidRow(
      textInput(inputId = ns("search_string"),
                label = "Search:"),
      checkboxGroupInput(
        inputId = ns("search_columns"),
        label = "Select columns to search in:",
        choices = NULL
      ),
      checkboxGroupInput(
        inputId = ns("display_columns"),
        label = "Select columns to display:",
        choices = NULL
      )
    ),
    fluidRow(
      column(width = 3,
             radioButtons(
               inputId = ns("search_type"),
               label = "",
               choices = c(
                 "Use SQLite Style Wildcard Characters",
                 "Use UNIX Style Wildcard Characters",
                 "Use Regex"
               )
             )),
      column(
        width = 4,
        conditionalPanel(
          condition = paste0(
            "input['",
            ns("search_type"),
            "'] == 'Use SQLite Style Wildcard Characters'"
          ),
          checkboxInput(inputId = ns("escape_characters"),
                        label = "Escape % and _ characters.")
        )
      )
    ),
    fluidRow(actionButton(
      inputId = ns("search_button"),
      label = "Search"
    )),
    fluidRow(uiOutput(ns(
      "search_results_ui"
    )))
  )
}

#' search Server Function
#'
#' @noRd

mod_search_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(data = NULL)
  
  output$search_results_ui <- renderUI({
    column(
      width = 12,
      id = "search_results",
      conditionalPanel(condition = !is.null(info$data),
                       fluidRow(DT::DTOutput(
                         ns("search_results")
                       )))
    )
  })
  
  output$search_results <-
    DT::renderDT(expr = {
      DT::datatable(data = info$data)
    })
  
  observeEvent(conn$active_table, {
    if (conn$active_table != "") {
      updateCheckboxGroupInput(
        session = session,
        inputId = "display_columns",
        choices = RSQLite::dbGetQuery(
          conn$active_db,
          table_structure_query(conn$active_table)
        )$name,
        selected = RSQLite::dbGetQuery(
          conn$active_db,
          table_structure_query(conn$active_table)
        )$name
      )
      updateCheckboxGroupInput(
        session = session,
        inputId = "search_columns",
        choices = RSQLite::dbGetQuery(
          conn$active_db,
          table_structure_query(conn$active_table)
        )$name,
        selected = RSQLite::dbGetQuery(
          conn$active_db,
          table_structure_query(conn$active_table)
        )$name
      )
    }
  })
  
  observeEvent(input$search_button, {
    if (input$search_string == "") {
      showNotification(ui = "Please enter a search query.",
                       duration = 3,
                       type = "error")
    }
    else if (is.null(input$display_columns))
      showNotification(ui = "Please select columns to display.",
                       duration = 3,
                       type = "error")
    else if (input$search_type == "Use SQLite Style Wildcard Characters") {
      info$data <- RSQLite::dbGetQuery(
        conn$active_db,
        search_query_sqlite(
          input$display_columns,
          input$search_columns,
          conn$active_table,
          input$search_string,
          input$escape_characters
        )
      )
    }
    else if (input$search_type == "Use UNIX Style Wildcard Characters") {
      info$data <- RSQLite::dbGetQuery(
        conn$active_db,
        search_query_unix(
          input$display_columns,
          input$search_columns,
          conn$active_table,
          input$search_string
        )
      )
    }
    else if (input$search_type == "Use Regex") {
      RSQLite::initRegExp(conn$active_db)
      info$data <- RSQLite::dbGetQuery(
        conn$active_db,
        search_query_regex(
          input$display_columns,
          input$search_columns,
          conn$active_table,
          input$search_string
        )
      )
    }
  })
}

## To be copied in the UI
# mod_search_ui("search_ui_1")

## To be copied in the server
# callModule(mod_search_server, "search_ui_1")

