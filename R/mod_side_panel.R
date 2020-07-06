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
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "RSQLiteAdmin",
                                    tags$li(
                                      class = "dropdown",
                                      tags$li(
                                        class = "dropdown",
                                        shinyFiles::shinyDirButton(
                                          id = ns("set_directory"),
                                          label = "Set database directory",
                                          title = "Select a folder",
                                          icon("paper-plane"),
                                          style = "color: #fff;
                                                       padding: 8.4%;
                                                       background-color: #337ab7;
                                                       border-color: #2e6da4"
                                        )
                                      )
                                    )),
    shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput(ns("sidebar_ui"))),
    shinydashboard::dashboardBody(mod_manage_dashboard_body_ui("manage_dashboard_body"))
    
  )
}

#' side_panel Server Function
#'
#' @noRd
mod_side_panel_server <-
  function(input,
           output,
           session,
           action,
           action_manage_tables,
           action_query) {
    ns <- session$ns
    
    
    
    # conn - stores the information about database
    # conn$active_db - the current active database selected by the user.
    # conn$db_name - string containing the name of current active database.
    # conn$active_table - the current active table selected by user.
    # conn$directory - string containing path to the current directory where databases are saved and imported.
    
    conn <- reactiveValues(
      active_db = NULL,
      db_name = "a34n4wi4nsi1sf39dvb",
      active_table = NULL,
      directory = NULL,
      db_list  = NULL,
      state = NULL
    )
    
    observeEvent(session, {
      conn$db_list <- db_list(conn$directory)
    })
    
    output$sidebar_ui <- shinydashboard::renderMenu({
      db_menu <- list()
      for (i in seq_len(length(conn$db_list))) {
        db_menu[[i]] <-
          convertMenuItem(
            shinydashboard::menuItem(
              text = conn$db_list[i],
              tabName = paste0("db_", i),
              icon = icon("search", lib = "glyphicon")
            ),
            paste0("db_", i)
          )
      }
      return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
    })
    
    observeEvent(input$sidebar_menu, {
      # print(conn$active_db)
      
      if (isTRUE(grepl("db", input$sidebar_menu, ignore.case = TRUE)))
      {
        selected_db_index <- strtoi(substr(
          input$sidebar_menu,
          start = 4,
          stop = nchar(input$sidebar_menu)
        ))
        selected_db <- conn$db_list[selected_db_index]
        
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
          
          table_list <- RSQLite::dbListTables(conn$active_db)
          
          # print(conn$db_list[1])
          db_menu <- list()
          for (i in seq_len(length(conn$db_list))) {
            if (conn$db_list[i] == selected_db &&
                !identical(table_list, character(0)))
            {
              db_menu[[i]] <-
                convertMenuItem(
                  shinydashboard::menuItem(
                    text = conn$db_list[i],
                    tabName = paste0("db_", i),
                    icon = icon("search", lib = "glyphicon"),
                    startExpanded = TRUE,
                    lapply(1:length(table_list), function(i) {
                      shinydashboard::menuSubItem(text = table_list[i],
                                                  tabName = paste0("table_", i))
                    })
                  ),
                  paste0("db_", i)
                )
            }
            else{
              db_menu[[i]] <-
                convertMenuItem(
                  shinydashboard::menuItem(
                    text = conn$db_list[i],
                    tabName = paste0("db_", i),
                    icon = icon("search", lib = "glyphicon")
                  ),
                  paste0("db_", i)
                )
            }
          }
          
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
      
      if (isTRUE(grepl("table", input$sidebar_menu, ignore.case = TRUE)))
        conn$state <- "Table"
      else
        conn$state <- "Database"
    })
    
    # Select active database and establish an RSQLite connection.
    
    # observeEvent(input$select_active_db, {
    #   if (!is.null(conn$directory)) {
    #     tryCatch({
    #       if (!is.null(conn$active_db)) {
    #         RSQLite::dbDisconnect(conn$active_db)
    #         conn$active_db <- NULL
    #         conn$db_name <- NULL
    #       }
    #       conn$active_db <-
    #         RSQLite::dbConnect(RSQLite::SQLite(),
    #                            paste0(conn$directory, input$select_active_db))
    #       conn$db_name <- input$select_active_db
    #
    #       if (!is.null(conn$active_db)) {
    #         updateSelectInput(
    #           session =  session,
    #           inputId =  "select_active_table",
    #           choices = RSQLite::dbListTables(conn$active_db)
    #         )
    #       }
    #     },
    #     error = function(err) {
    #       showNotification(
    #         ui =  paste0("No databases in this folder. Create or import one."),
    #         duration = 3,
    #         type = "warning"
    #       )
    #     })
    #   }
    # })
    
    
    
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
        conn$db_list <- db_list(conn$directory)
        if (length(conn$db_list) == 0) {
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            db_menu[[1]] <- shinydashboard::menuItem(text = "No databases in current folder.",
                                                     icon = icon("search", lib = "glyphicon"))
            return(shinydashboard::sidebarMenu(id = ns("sidebar_menu"), db_menu))
          })
        }
        else{
          output$sidebar_ui <- shinydashboard::renderMenu({
            db_menu <- list()
            for (i in seq_len(length(conn$db_list))) {
              db_menu[[i]] <-
                convertMenuItem(
                  shinydashboard::menuItem(
                    text = conn$db_list[i],
                    tabName = paste0("db_", i),
                    icon = icon("search", lib = "glyphicon")
                  ),
                  paste0("db_", i)
                )
            }
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
      updateSelectInput(
        session,
        inputId =  "select_active_db",
        label = "Choose a database",
        choices = db_list(conn$directory)
      )
    })
    
    
    
    
    # Select active table and set that value to conn$active_table
    
    observeEvent(input$select_active_table, {
      conn$active_table <- input$select_active_table
    })
    
    # Update database list when a database is deleted
    
    observeEvent(action$deleted_db, {
      updateSelectInput(session,
                        inputId =  "select_active_db",
                        choices = db_list(conn$directory))
    })
    
    # Update table list when a new table is created
    
    observeEvent(action_manage_tables$created_table, {
      updateSelectInput(
        session,
        inputId =  "select_active_table",
        choices = RSQLite::dbListTables(conn$active_db)
      )
    })
    
    # Update table list when a table is dropped.
    
    observeEvent(action_manage_tables$dropped_table, {
      updateSelectInput(
        session,
        inputId =  "select_active_table",
        choices = RSQLite::dbListTables(conn$active_db)
      )
    })
    
    # Update table list when a table is renamed.
    
    observeEvent(action_manage_tables$renamed_table, {
      updateSelectInput(
        session,
        inputId =  "select_active_table",
        choices = RSQLite::dbListTables(conn$active_db)
      )
    })
    
    # Update database list when a query is executed
    
    observeEvent(action_query$data_updated, {
      tryCatch({
        updateSelectInput(
          session,
          inputId =  "select_active_db",
          label = "Choose a database",
          choices = db_list(conn$directory)
        )
      })
    })
    
    # Update table list when a query is executed.
    
    observeEvent(action_query$data_updated, {
      tryCatch({
        updateSelectInput(
          session,
          inputId =  "select_active_table",
          choices = RSQLite::dbListTables(conn$active_db)
        )
      })
    })
    
    # Return the conn reactive values
    
    return(conn)
  }

## To be copied in the UI
# mod_side_panel_ui("side_panel_ui_1")

## To be copied in the server
# callModule(mod_side_panel_server, "side_panel_ui_1")
