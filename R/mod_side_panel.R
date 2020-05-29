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
  uiOutput(ns("db_list_control"))
}

#' side_panel Server Function
#'
#' @noRd
mod_side_panel_server <-
  function(input, output, session) {
    ns <- session$ns
    
    conn <- reactiveValues(
      active_db = NULL,
      db_name = NULL,
      active_table = NULL,
      directory = "./Databases/"
    )
    
    output$db_list_control <- renderUI({
      fluidPage(fluidRow(
        actionButton(inputId =  ns("create_db"), label =  "Create a new database"),
        br(),
        br(),
        selectInput(
          ns("select_active_db"),
          "Select a database to work on",
          choices = db_list(directory = conn$directory)
        ),
        selectInput(
          ns("select_active_table"),
          "Select a table to work on.",
          choices = NULL
        )
      ))
    })
    
    observeEvent(input$create_db, {
      if (is.null(conn$directory))
        showNotification(ui = "Please set a directory to store databases first.",
                         duration = 3,
                         type = "error")
      else
        showModal(modalDialog(easyClose = TRUE, fluidRow(
          column(
            width = 12,
            offset = 1,
            textInput(
              inputId =  ns("new_db_name"),
              label = "Create a new database",
              placeholder = "Your database name here"
            ),
            actionButton(inputId = ns("confirm_db_name"), label = "Create Database")
          )
        )))
    })
    
    observeEvent(input$confirm_db_name, {
      if (input$new_db_name == "") {
        showNotification(ui = "Please input database name to create database.",
                         duration = 3,
                         type = "error")
      }
      else if (paste0(input$new_db_name, ".db") %in% db_list(conn$directory)) {
        showNotification(ui =  "Database with this name already exists. Please specify another name.",
                         duration = 5,
                         type = "error")
      }
      else {
        create_db(input$new_db_name, conn$directory)
        showNotification(ui = "The database was created successfully!",
                         duration = 3,
                         type = "message")
      }
      updateSelectInput(
        session,
        inputId =  "select_active_db",
        label = "Choose a database",
        choices = db_list(conn$directory)
      )
    })
    
    observeEvent(input$select_active_db, {
      if (!is.null(conn$directory)) {
        if (!is.null(conn$active_db)) {
          RSQLite::dbDisconnect(conn$active_db)
        }
        conn$active_db <-
          RSQLite::dbConnect(RSQLite::SQLite(),
                             paste0(conn$directory, input$select_active_db))
        conn$db_name <- input$select_active_db
        if (!is.null(conn$active_db)) {
          updateSelectInput(
            session =  session,
            inputId =  "select_active_table",
            choices = RSQLite::dbListTables(conn$active_db)
          )
        }
      }
    })
    
    observeEvent(input$select_active_table, {
      conn$active_table <- input$select_active_table
    })
    
    return(conn)
  }

## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")

## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")