#' dashboard_structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @import shinyFiles
#' @import RSQLite
#' @import shinydashboard
#' @importFrom fs path_home

mod_dashboard_structure_ui <- function(id) {
  ns <- NS(id)
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "RSQLiteAdmin",
      tags$li(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        class = "dropdown",
        shinyFiles::shinyDirButton(
          id = ns("set_directory"),
          label = "Set database directory",
          title = "Select a folder"
        )
      ),
      tags$li(
        class = "dropdown",
        actionButton(
          inputId =  ns("create_db"),
          label =  "Create a new database",
          icon("paper-plane")
        )
      )
    ),
    shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput(ns("sidebar_ui"))),
    shinydashboard::dashboardBody(
      mod_manage_dashboard_body_ui("manage_dashboard_body"),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      )
    )
  )
}

#' dashboard_structure Server Function
#'
#' @noRd

mod_dashboard_structure_server <-
  function(input,
           output,
           session,
           action,
           action_manage_tables,
           action_query,
           action_create_table,
           action_import_tables,
           action_clone_tables) {
    ns <- session$ns
    
    # conn - stores the information about database
    # conn$active_db - the current active database selected by the user.
    # conn$db_name - string containing the name of current active database. To
    #                support current functionality, right now a
    #                random string has been assigned to this variable.
    # conn$active_table - the current active table selected by user.
    # conn$directory - string containing path to the current directory
    # where databases are saved and imported.
    # conn$db_list - List of all databases in current directory
    # conn$state - Stores if a Database or a Table is selected
    #              currently so that tabs according to that can be shown.
    
    conn <- reactiveValues(
      active_db = NULL,
      db_name = "a34n4wi4nsi1sf39dvbKNFDIDN",
      active_table = NULL,
      directory = NULL,
      db_list  = NULL,
      state = NULL
    )
    
    observeEvent(session, {
      conn$db_list <- db_list(conn$directory)
    })
    
    # Load the list of databases initially on starting the app
    
    output$sidebar_ui <- shinydashboard::renderMenu({
      db_menu <- update_sidebar_db(conn$db_list)
      return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
    })
    
    
    
    # Select active database/active table and establish an RSQLite connection.
    
    observeEvent(input$sidebar_menu, {
      # print(input$sidebarItemExpanded)
      if (isTRUE(grepl("db", input$sidebar_menu, ignore.case = TRUE)))
      {
        # print("db grepled")
        selected_db_index <- strtoi(substr(
          input$sidebar_menu,
          start = 4,
          stop = nchar(input$sidebar_menu)
        ))
        selected_db <- conn$db_list[selected_db_index]
        print(selected_db)
        
        # conn$active_table has to be set to NULL because viewing tables
        # depends on it. So say if there are two databases with a table
        # of same name but different data, and if you switch back and
        # forth between those tables, then data won't be refreshed
        # unless conn$active_table is changed. Since switching between
        # those tables would first require to switch between databases
        # in order to tables to be displayed, therefore this is the 
        # optimum place to set it to NULL. Also, changing it to NULL
        # when a database has been clicked on is O.K. since no operations
        # when a database is selected depend on conn$active_table.
        
        # In the future too, for this to be compatible, no operations
        # when a database is selected should depend on conn$active_table.
        conn$active_table <- NULL
        
        if (conn$db_name != selected_db) {
          if (!is.null(conn$directory)) {
            tryCatch({
              if (!is.null(conn$active_db)) {
                RSQLite::dbDisconnect(conn$active_db)
                conn$active_db <- NULL
                conn$db_name <- NULL
              }
              
              conn$active_db <-
                RSQLite::dbConnect(RSQLite::SQLite(),
                                   paste0(conn$directory, selected_db))
              conn$db_name <- selected_db
            },
            error = function(err) {
              showNotification(
                ui =  paste0("No databases in this folder. Create or import one."),
                duration = 3,
                type = "warning"
              )
            })
          }
          
          db_menu <-
            update_sidebar_table(input$sidebar_menu, conn$active_db, conn$db_list)
          
          conn$db_name <- selected_db
          output$sidebar_ui <-
            shinydashboard::renderMenu({
              shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
            })
          shinydashboard::updateTabItems(session,
                                         inputId = 'sidebar_menu',
                                         selected = input$sidebar_menu)
        }
      }
      
      if (isTRUE(grepl("table", input$sidebar_menu, ignore.case = TRUE))) {
        table_list <- RSQLite::dbListTables(conn$active_db)
        selected_table_index <- strtoi(substr(
          input$sidebar_menu,
          start = 7,
          stop = nchar(input$sidebar_menu)
        ))
        conn$active_table <- table_list[selected_table_index]
      }
      
      if (isTRUE(grepl("table", input$sidebar_menu, ignore.case = TRUE)))
        conn$state <- "Table"
      else
        conn$state <- "Database"
    })
    
    # Select directory to save and import databases
    # Current user selected directory is store in ./inst/extdata/directory.Rdata
    # Inside the directory.Rdata file, directory is stored in variable named db_directory_path.
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
        conn$db_list <- db_list(conn$directory)
        if (length(conn$db_list) == 0) {
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            db_menu[[1]] <-
              shinydashboard::menuItem(text = "No databases in current folder.",
                                       icon = icon("search", lib = "glyphicon"))
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
        else{
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- update_sidebar_db(conn$db_list)
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
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
      conn$db_list <- db_list(conn$directory)
      if (length(conn$db_list) == 0) {
        output$sidebar_ui <- shinydashboard::renderMenu({
          db_menu <- list()
          db_menu[[1]] <-
            shinydashboard::menuItem(text = "No databases in current folder.",
                                     icon = icon("search", lib = "glyphicon"))
          return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
        })
      }
      else{
        output$sidebar_ui <- shinydashboard::renderMenu({
          db_menu <- update_sidebar_db(conn$db_list)
          return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
        })
      }
    })
    
    # Update database list when a database is deleted
    
    observeEvent(action$deleted_db, {
      conn$db_list <- db_list(conn$directory)
      if (length(conn$db_list) == 0) {
        output$sidebar_ui <- shinydashboard::renderMenu({
          db_menu <- list()
          db_menu[[1]] <-
            shinydashboard::menuItem(text = "No databases in current folder.",
                                     icon = icon("search", lib = "glyphicon"))
          return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
        })
      }
      else{
        output$sidebar_ui <- shinydashboard::renderMenu({
          db_menu <- update_sidebar_db(conn$db_list)
          return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
        })
      }
    })
    
    # Update table list when a new table is created
    
    observeEvent(action_create_table$created_table, {
      db_menu <-
        update_sidebar_table(input$sidebar_menu, conn$active_db, conn$db_list)
      output$sidebar_ui <-
        shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
        })
      shinydashboard::updateTabItems(session,
                                     inputId = 'sidebar_menu',
                                     selected = input$sidebar_menu)
    })
    
    # Update table list when a table is dropped.
    
    observeEvent(action_manage_tables$dropped_table, {
      db_menu <- update_sidebar_db(conn$db_list)
      output$sidebar_ui <-
        shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
        })
    })
    
    # Update table list when a table is renamed.
    
    observeEvent(action_manage_tables$renamed_table, {
      db_menu <- update_sidebar_db(conn$db_list)
      output$sidebar_ui <-
        shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
        })
      shinydashboard::updateTabItems(session,
                                     inputId = 'sidebar_menu',
                                     selected = input$sidebar_menu)
    })
    
    # Update database list when a query is executed
    
    observeEvent(action_query$data_updated, {
      tryCatch({
        conn$db_list <- db_list(conn$directory)
        if (length(conn$db_list) == 0) {
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            db_menu[[1]] <-
              shinydashboard::menuItem(text = "No databases in current folder.",
                                       icon = icon("search", lib = "glyphicon"))
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
        else{
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- update_sidebar_db(conn$db_list)
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
            shinydashboard::updateTabItems(session,
                                           inputId = 'sidebar_menu',
                                           selected = input$sidebar_menu)
          })
        }
      })
    })
    
    observeEvent(action_query$data_updated_save, {
      tryCatch({
        conn$db_list <- db_list(conn$directory)
        if (length(conn$db_list) == 0) {
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            db_menu[[1]] <-
              shinydashboard::menuItem(text = "No databases in current folder.",
                                       icon = icon("search", lib = "glyphicon"))
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
        else{
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- update_sidebar_db(conn$db_list)
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
            shinydashboard::updateTabItems(session,
                                           inputId = 'sidebar_menu',
                                           selected = input$sidebar_menu)
          })
        }
      })
    })
    
    observeEvent(action_query$data_updated_recent, {
      tryCatch({
        conn$db_list <- db_list(conn$directory)
        if (length(conn$db_list) == 0) {
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            db_menu[[1]] <-
              shinydashboard::menuItem(text = "No databases in current folder.",
                                       icon = icon("search", lib = "glyphicon"))
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
        else{
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- update_sidebar_db(conn$db_list)
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
            shinydashboard::updateTabItems(session,
                                           inputId = 'sidebar_menu',
                                           selected = input$sidebar_menu)
          })
        }
      })
    })
    
    # Update table list when a new table is imported
    
    observeEvent(action_import_tables$imported_tables, {
      db_menu <-
        update_sidebar_table(input$sidebar_menu, conn$active_db, conn$db_list)
      output$sidebar_ui <-
        shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
        })
      shinydashboard::updateTabItems(session,
                                     inputId = 'sidebar_menu',
                                     selected = input$sidebar_menu)
    }, ignoreInit = TRUE)
    
    # Update table list when tables are cloned
    
    observeEvent(action_clone_tables$tables_cloned, {
      db_menu <-
        update_sidebar_table(input$sidebar_menu, conn$active_db, conn$db_list)
      output$sidebar_ui <-
        shinydashboard::renderMenu({
          shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu)
        })
      shinydashboard::updateTabItems(session,
                                     inputId = 'sidebar_menu',
                                     selected = input$sidebar_menu)
    }, ignoreInit = TRUE)
    
    # Return the conn reactive values
    
    return(conn)
  }

## To be copied in the UI
# mod_dashboard_structure_ui("dashboard_structure_ui_1")

## To be copied in the server
# callModule(mod_dashboard_structure_server, "dashboard_structure_ui_1")

