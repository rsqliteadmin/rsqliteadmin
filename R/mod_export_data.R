#' export_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_export_data_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Export Data",
    br(),
    fluidRow(
      column(
        width = 2,
        shinyFiles::shinyDirButton(
          id = ns("save_directory"),
          label = "Saving Directory",
          title = "Select a folder"
        )
      ),
      column(width = 10,
             verbatimTextOutput(ns(
               "directory_selected"
             )))
    ),
    fluidRow(column(
      width = 4,
      textInput(
        inputId = ns("delimiter"),
        label = "Separator",
        value = ","
      )
    ),
    column(
      width = 4,
      textInput(
        inputId = ns("missing_values_string"),
        label = "String used for missing values.",
        value = "NA"
      )
    )),
    checkboxGroupInput(inputId = ns("selected_tables"),
                       label = "Select table(s) to export."),
    selectInput(
      inputId = ns("table_list"),
      label = "Edit Properties of: ",
      choices = NULL
    ),
    textInput(inputId = ns("file_name"),
              label = "File Name"),
    checkboxGroupInput(inputId = ns("selected_columns"),
                       label = "Select Columns to export."),
    fluidRow(checkboxInput(
      inputId = ns("include_column_names"),
      label = "Include Column Names"
    )),
    actionButton(inputId = ns("export"),
                 label = "Export")
  )
}

#' export_data Server Function
#'
#' @noRd
mod_export_data_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  # info$file_name_list - List of file names whom data will be exported to,
  #                       default for each file is the corresponding table name.
  # info$column_list - For each table, the columns that have to be exported.
  # info$include_column_names - For each table, specifies if column names are to
  #                             be included in the exported data.
  #
  
  info <- reactiveValues(
    file_name_list = list(),
    column_list = list(),
    include_column_names = list(),
    delimiter = NULL,
    save_directory = NULL
  )
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyDirChoose(input = input,
                             id = "save_directory",
                             roots = roots)
  
  output$directory_selected <- renderText({
    return(info$save_directory)
  })
  
  observeEvent(conn$active_db, {
    if (!is.null(conn$active_db)) {
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_tables",
        choices = RSQLite::dbListTables(conn$active_db)
      )
      for (i in RSQLite::dbListTables(conn$active_db)) {
        info$file_name_list[[i]] = i
        info$column_list[[i]] = RSQLite::dbGetQuery(conn$active_db,
                                                    table_structure_query(i))$name
        info$include_column_names[[i]] = TRUE
      }
    }
  })
  
  observeEvent(input$save_directory, {
    path <-
      shinyFiles::parseDirPath(roots = roots, input$save_directory)
    if (!(identical(path, character(0))))
      info$save_directory <- paste0(path, "/")
  })
  
  observeEvent(input$selected_tables, {
    updateSelectInput(
      session = session,
      inputId = "table_list",
      choices = input$selected_tables
    )
  })
  
  observeEvent(input$table_list, {
    updateTextInput(
      session = session,
      inputId = "file_name",
      value = info$file_name_list[[input$table_list]]
    )
    updateCheckboxGroupInput(
      session = session,
      inputId = "selected_columns",
      choices = RSQLite::dbGetQuery(conn$active_db,
                                    table_structure_query(input$table_list))$name,
      selected = info$column_list[[input$table_list]]
    )
    updateCheckboxInput(
      session = session,
      inputId = "include_column_names",
      value = info$include_column_names[[input$table_list]]
    )
  })
  
  observeEvent(input$file_name, {
    info$file_name_list[[input$table_list]] <- input$file_name
  })
  
  observeEvent(input$selected_columns, {
    if (!is.null(input$table_list)) {
      info$column_list[[input$table_list]] <- input$selected_columns
    }
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$include_column_names, {
    info$include_column_names[[input$table_list]] <-
      input$include_column_names
  })
  
  observeEvent(input$delimiter, {
    tryCatch({
      # To parse delimiters of the form "\t", "\n", "\r" - these
      # are read in by shiny as "\\t", "\\n, "\\r" because the
      # slash is escaped. We remove the first slash used for
      # escaping so that it can be passed on as a valid delimiter.
      if (isTRUE(grepl("\\", input$delimiter, fixed = TRUE)))
        info$delimiter <- eval(parse(text = sub(
          "\\", "", deparse(input$delimiter), fixed = TRUE
        )))
      else
        info$delimiter <- input$delimiter
    },
    error = function(err) {
      info$header_data <- NULL
      showNotification(
        ui =  paste0("Please enter a valid separator."),
        duration = 3,
        type = "error"
      )
    })
  })
  
  observeEvent(input$export, {
    if (is.null(info$save_directory))
      showNotification(ui = "Please select a directory where files are to be saved.",
                       duration = 3,
                       type = "error")
    else if (is.null(info$delimiter))
      sshowNotification(ui = "Please enter a valid separator.",
                        duration = 10,
                        type = "error")
    else{
      tryCatch({
        data <- NULL
        extension <- NULL
        if (info$delimiter == "," || info$delimiter == ";")
          extension <- "csv"
        else if (identical(info$delimiter, "\t"))
          extension <- "tsv"
        else
          extension <- "txt"
        for (i in input$selected_tables) {
          offset <- 0
          if (!is.null(info$column_list[[i]]))
          {
            data <- RSQLite::dbGetQuery(
              conn$active_db,
              export_data_fetch_query(i,
                                      1000,
                                      offset,
                                      info$column_list[[i]])
            )
            file_path <-
              paste0(info$save_directory,
                     info$file_name_list[[i]],
                     ".",
                     extension)
            # Never overwrite a file, always append.
            if (isTRUE(info$include_column_names[[i]])) {
              readr::write_delim(
                x = data,
                path = file_path,
                delim = info$delimiter,
                append = TRUE,
                col_names = TRUE,
                na = input$missing_values_string
              )
            }
            else{
              readr::write_delim(
                x = data,
                path = file_path,
                delim = info$delimiter,
                append = TRUE,
                col_names = FALSE,
                na = input$missing_values_string
              )
            }
            while (TRUE) {
              offset <- offset + 1000
              data <- RSQLite::dbGetQuery(
                conn$active_db,
                export_data_fetch_query(i,
                                        1000,
                                        offset,
                                        info$column_list[[i]])
              )
              readr::write_delim(
                x = data,
                path = file_path,
                delim = info$delimiter,
                append = TRUE,
                col_names = FALSE,
                na = input$missing_values_string
              )
              if (nrow(data) < 1000)
                break
            }
          }
          else
            showNotification(
              ui = paste0(
                " No columns selected for table ",
                i,
                ". Data for this table
                        not exported."
              ),
              duration = 10,
              type = "error"
            )
        }
      },
      error = function(err) {
        showNotification(
          ui = paste0(err, "Data not exported."),
          duration = 10,
          type = "error"
        )
      })
    }
  })
}

## To be copied in the UI
# mod_export_data_ui("export_data_ui_1")

## To be copied in the server
# callModule(mod_export_data_server, "export_data_ui_1")

