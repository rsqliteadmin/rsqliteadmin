#' side_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinyFiles
#' @import RSQLite
#' @importFrom fs path_home

mod_side_panel_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("side_panel_ui"))
}

#' side_panel Server Function
#'
#' @noRd
mod_side_panel_server <- function(input, output, session, action) {
  ns <- session$ns
  
  output$side_panel_ui <- renderUI({
    fluidPage(
      fluidRow(
        actionButton(inputId =  ns("create_db"), label =  "Create a new database"),
        br(),
        br(),
        shinyFiles::shinyDirButton(
          id = ns("set_directory"),
          label = "Set database directory",
          title = "Select a folder"
        ),
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
      )
    )
  })
  
  # conn - stores the information about database
  # conn$active_db - the current active database selected by the user.
  # conn$db_name - string containing the name of current active database.
  # conn$active_table - the current active table selected by user.
  # conn$directory - string containing path to the current directory where databases are saved and imported.
  
  conn <- reactiveValues(
    active_db = NULL,
    db_name = NULL,
    active_table = NULL,
    directory = NULL
  )
  
  # Select directory to save and import databases
  # Current user selected directory is store in ./inst/extdata/directory.Rdata
  # Default directory when the first time app is opened is the current working directory.
  
  load(
    system.file(
      "extdata",
      "directory.Rdata",
      package = "rsqliteadmin",
      mustWork = TRUE
    )
  )
  conn$directory <- db_directory_path
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyDirChoose(input = input,
                             id = "set_directory",
                             roots = roots)
  
  # parseDirPath returns character(0) on its first click.
  
  observeEvent(input$set_directory, {
    path <- shinyFiles::parseDirPath(roots = roots, input$set_directory)
    if (!(identical(path, character(0)))) {
      db_directory_path <- paste0(path, "/")
      conn$directory <- db_directory_path
      save(
        db_directory_path,
        file = system.file(
          "extdata",
          "directory.Rdata",
          package = "rsqliteadmin",
          mustWork = TRUE
        )
      )
    }
  })
  
  # Create a new database
  # Check that the database with the same name does not already exist.
  
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
  
  # Select active database and establish an RSQLite connection.
  
  observeEvent(input$select_active_db, {
    if (!is.null(conn$directory)) {
      tryCatch({
        if (!is.null(conn$active_db)) {
          RSQLite::dbDisconnect(conn$active_db)
          conn$active_db <- NULL
          conn$db_name <- NULL
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
      },
      error = function(err) {
        showNotification(
          ui =  paste0("No databases in this folder. Create or import one."),
          duration = 3,
          type = "warning"
        )
      })
    }
  })
  
  
  # Select active table and set that value to conn$active_table
  
  observeEvent(input$select_active_table, {
    conn$active_table <- input$select_active_table
  })
  
  # Update database list when a database is deleted
  
  observeEvent(action$deleted_db, {
    updateSelectInput(
      session,
      inputId =  "select_active_db",
      label = "Choose a database",
      choices = db_list(conn$directory)
    )
  })
  
  # Return the conn reactive values
  
  return(conn)
}

## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")

## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")
