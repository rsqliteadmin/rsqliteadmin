#' import_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' 
#' @import disk.frame
#' @import shinyFiles
#' @importFrom DT renderDT DTOutput
#' @importFrom DT JS datatable
#' @importFrom shiny NS
#' @importFrom fs path_home
#' @importFrom tools file_path_sans_ext
#' @importFrom RSQLite dbWriteTable
#' @importFrom data.table fread
#' @importFrom utils capture.output globalVariables
#' @importFrom magrittr %<>% 
 
mod_import_tables_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Import Tables",
    column(
      width = 12,
      fluidRow(column(width = 12,
                      h2(
                        "Import New Tables"
                      ))),
      fluidRow(
        column(
          width = 1,
          shinyFiles::shinyFilesButton(
            id = ns("file_button"),
            label = "Select File(s)",
            title = "Select File(s)",
            multiple = TRUE
          )
        ),
        column(width = 11,
               verbatimTextOutput(ns("file_selected")))
      ),
      fluidRow(column(
        width = 3,
        textInput(inputId = ns("delimiter"),
                  label = "Separator")
      ),
      column(
        width = 9,
        selectInput(
          inputId = ns("file_list"),
          label = "Edit Properties of:",
          choices = NULL
        )
      )),
      fluidRow(column(
        width = 4,
        textInput(
          inputId = ns("table_name"),
          label = "Table Name",
          placeholder = "Enter Table Name"
        )
      )),
      fluidRow(column(
        width = 4,
        checkboxInput(
          inputId = ns("column_names_present"),
          label = "File contains column names."
        )
      )),
      uiOutput(ns("display_header_ui")),
      br(),
      fluidRow(column(
        width = 12,
        actionButton(
          inputId = ns("checkbox_columns_button"),
          label = "Select from List"
        ),
        actionButton(
          inputId = ns("specify_columns_button"),
          label = "Select by Name"
        )
      )),
      br(),
      fluidRow(column(
        width = 1,
        actionButton(inputId = ns("import"),
                     label = "Import")
      ))
    ),
    br()
  )
}

#' import_tables Server Function
#'
#' @noRd
mod_import_tables_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(
    file_data = NULL,
    file_names = NULL,
    file_paths = list(),
    table_names = list(),
    import_type = list(),
    header_selected_columns = list(),
    header_selected_columns_index = list(),
    checkbox_selected_columns = list(),
    specify_columns = list(),
    column_names_present = list(),
    header_data = NULL,
    delimiter = NULL
  )
  
  action_import_tables <- reactiveValues(imported_tables = 0)
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "file_button",
                              roots = roots)
  
  
  
  output$display_header_ui <- renderUI({
    column(width = 12,
           fluidRow(
             conditionalPanel(
               condition = !is.null(info$file_data),
               column(width = 12,
                      DT::DTOutput(ns(
                        "display_header"
                      ))),
               fluidRow(column(width = 12,
                               br(),
                               fluidRow(
                                 column(
                                   width = 12,
                                   radioButtons(
                                     inputId = ns("import_type"),
                                     label = "Import from file:",
                                     choices = c(
                                       "All Columns",
                                       "Columns Selected in Table",
                                       "Columns from List",
                                       "Columns by Name"
                                     )
                                   )
                                 )
                               )))
             )
           ))
  })
  
  output$display_header <- DT::renderDT(expr = {
    DT::datatable(
      data = info$header_data,
      selection = list(target = "column", selected = NULL),
      plugins = "ellipsis",
      options = list(dom = 't',
                     columnDefs = list(
                       list(
                         targets = "_all",
                         render = DT::JS("$.fn.dataTable.render.ellipsis(20)")
                       )
                     ))
    )
  })
  
  output$file_selected <- renderText({
    if (is.null(info$file_data))
      return("")
    else
    {
      res <- ""
      for (i in seq_len(dim(info$file_data)[1]))
      {
        res <- paste0(res, info$file_data$datapath[i], "\n")
      }
      return(res)
    }
  })
  
  observeEvent(input$table_name, {
    if (input$file_list != "") {
      info$table_names[[input$file_list]] <- input$table_name
    }
  })
  
  observeEvent(input$delimiter, {
    tryCatch({
      if (isTRUE(grepl("\\", input$delimiter, fixed = TRUE)))
        info$delimiter <- eval(parse(text = sub(
          "\\", "", deparse(input$delimiter), fixed = TRUE
        )))
      else
        info$delimiter <- input$delimiter
      
      info$header_data <-
        data.table::fread(info$file_paths[[input$file_list]],
                          nrows = 5,
                          sep = info$delimiter)
    },
    error = function(err) {
      info$header_data <- NULL
      showNotification(
        ui =  paste0("Please enter a valid separator."),
        duration = 3,
        type = "error"
      )
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$file_button, {
    tryCatch({
      paths <-
        shinyFiles::parseFilePaths(roots = roots, input$file_button)
      if (dim(paths)[1] != 0)
        info$file_data <- paths
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". File couldn't be selected."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  # Not declaring this causes a NOTE in R CMD Check. Do not remove.
  # Reference here: https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
  utils::globalVariables(c("."))
  
  observeEvent(info$file_data, {
    if (!is.null(info$file_data)) {
      tryCatch({
        if (length(info$file_data)[1] != 0) {
          info$file_names <- info$file_data$name
          
          updateSelectInput(
            session = session,
            inputId = "file_list",
            choices = info$file_names
          )
          
          # Reset all data in lists
          info$table_names <- NULL
          info$file_paths <- NULL
          info$import_type <- NULL
          info$header_selected_columns = list()
          info$checkbox_selected_columns = list()
          info$specify_columns = list()
          info$column_names_present = list()
          
          # By default, import type for every table is all columns.
          j = 1
          for (i in info$file_names) {
            info$table_names[[i]] <- tools::file_path_sans_ext(i)
            info$file_paths[[i]] <- info$file_data$datapath[[j]]
            info$import_type[[i]] <- "All Columns"
            info$header_selected_columns[[i]] <- NULL
            info$checkbox_selected_columns[[i]] <- NULL
            info$specify_columns[[i]] <- NULL
            info$column_names_present[[i]] <- TRUE
            j = j + 1
          }
          
          # Use the first selected file to determine separator.
          # Separator is common for all files to be imported.
          # Reference here: https://stackoverflow.com/a/50121613/
          # Reference here: https://stackoverflow.com/a/16563900/
          withProgress(message = "Processing File", expr =  {
            fread_output <-
              utils::capture.output(data.table::fread(info$file_data$datapath[1], verbose = TRUE) %>% {
                NULL
              }) %>%
              .[grepl('%) sep=', .)]
            sep_regex <-
              regexec("\\s+\\d+\\.\\d+\\w* \\([ ]*\\d+%\\) sep='(.{1,3})'",
                      fread_output)
            sep_capture_groups <-
              regmatches(fread_output, sep_regex)
            delimiter <- sep_capture_groups[[1]][2]
          })
          # if (identical(delimiter, "\t"))
          #   info$delimiter <- "\\t"
          # else
          #   info$delimiter <- delimiter
          if (isTRUE(grepl("\\", delimiter, fixed = TRUE)))
            info$delimiter <- eval(parse(text = sub(
              "\\", "", deparse(delimiter), fixed = TRUE
            )))
          else
            info$delimiter <- delimiter
          updateTextInput(
            session = session,
            inputId = "delimiter",
            value = info$delimiter
          )
        }
      },
      error = function(err) {
        showNotification(
          ui =  paste0(err, ". Cannot show header."),
          duration = 5,
          type = "error"
        )
      })
    }
  })
  
  observeEvent(input$file_list, {
    if (input$file_list != "") {
      withProgress(message = "Processing File", expr =  {
        info$header_data <-
          data.table::fread(
            info$file_paths[[input$file_list]],
            nrows = 5,
            sep = info$delimiter,
            header = info$column_names_present[[input$file_list]]
          )
      })
      
      output$display_header <- DT::renderDT(expr = {
        DT::datatable(
          data = info$header_data,
          selection = list(
            target = "column",
            selected = info$header_selected_columns[[input$file_list]]
          ),
          plugins = "ellipsis",
          options = list(dom = 't',
                         columnDefs = list(
                           list(
                             targets = "_all",
                             render = DT::JS("$.fn.dataTable.render.ellipsis(20)")
                           )
                         ))
        )
      })
      
      updateTextInput(
        session = session,
        inputId = "table_name",
        value = info$table_names[[input$file_list]]
      )
      
      updateRadioButtons(
        session = session,
        inputId = "import_type",
        selected = info$import_type[[input$file_list]]
      )
      
      updateCheckboxInput(
        session = session,
        inputId = "column_names_present",
        value = info$column_names_present[[input$file_list]]
      )
    }
    
  })
  
  observeEvent(input$column_names_present, {
    info$header_data <- NULL
    
    if (input$file_list != "") {
      info$column_names_present[[input$file_list]] <-
        input$column_names_present
      
      withProgress(message = "Processing File", expr =  {
        info$header_data <-
          data.table::fread(
            info$file_paths[[input$file_list]],
            nrows = 5,
            sep = info$delimiter,
            header = info$column_names_present[[input$file_list]]
          )
      })
    }
  })
  
  observeEvent(
    input$display_header_columns_selected,
    {
      if (input$file_list != "") {
        info$header_selected_columns[[input$file_list]] <-
          input$display_header_columns_selected
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  
  observeEvent(input$import_type, {
    if (input$file_list != "")
      info$import_type[[input$file_list]] <- input$import_type
  }, ignoreInit = TRUE)
  
  observeEvent(input$checkbox_columns_button, {
    if (input$file_list == "")
      showNotification(
        ui =  paste0("No file selected."),
        duration = 3,
        type = "error"
      )
    else {
      showModal(
        modalDialog(
          size = "l",
          title = "Select Columns to Import",
          checkboxGroupInput(
            inputId = ns("checkbox_columns"),
            label = "Select columns to import",
            choices = colnames(info$header_data),
            selected = info$checkbox_selected_columns[[input$file_list]]
          ),
          actionButton(inputId = ns("select_all"),
                       label = "Select/Deselect All")
        )
      )
    }
  })
  
  observeEvent(input$select_all, {
    if (input$select_all %% 2 == 0)
      updateCheckboxGroupInput(
        session = session,
        inputId = "checkbox_columns",
        choices = colnames(info$header_data)
      )
    else
      updateCheckboxGroupInput(
        session = session,
        inputId = "checkbox_columns",
        choices = colnames(info$header_data),
        selected = colnames(info$header_data)
      )
  })
  
  observeEvent(input$checkbox_columns,
               {
                 info$checkbox_selected_columns[[input$file_list]] <-
                   input$checkbox_columns
               },
               ignoreNULL = FALSE,
               ignoreInit = TRUE)
  
  observeEvent(input$specify_columns_button, {
    if (input$file_list == "")
      showNotification(
        ui =  paste0("No file selected."),
        duration = 3,
        type = "error"
      )
    else {
      showModal(
        modalDialog(
          size = "l",
          title = "Specify Column Names",
          textInput(
            inputId = ns("specify_columns"),
            label = "Specify Column Names separated by a comma.",
            placeholder = "col1, col2, col3, col4",
            value = info$specify_columns[[input$file_list]]
          ),
          actionButton(
            inputId = ns("confirm_specify_columns"),
            label = "Confirm"
          )
        )
      )
    }
  })
  
  observeEvent(input$confirm_specify_columns, {
    info$specify_columns[[input$file_list]] <- input$specify_columns
    removeModal()
  })
  
  observeEvent(input$import, {
    if (is.null(info$file_data)) {
      showNotification(ui =  "No file selected.",
                       duration = 5,
                       type = "error")
    }
    else{
      for (i in info$file_names) {
        if (info$import_type[[i]] == "All Columns") {
          tryCatch({
            withProgress(message = "Import in Progress", expr =  {
              temp_df <-
                disk.frame::csv_to_disk.frame(
                  infile = info$file_paths[[i]],
                  sep = info$delimiter,
                  header = info$column_names_present[[i]]
                )
              chunk_ids <- disk.frame::get_chunk_ids(temp_df)
              for (chunk in chunk_ids) {
                temp_chunk <-
                  disk.frame::get_chunk(temp_df, as.numeric(chunk))
                # Never overwrite data, always append.
                RSQLite::dbWriteTable(conn$active_db,
                                      info$table_names[[i]],
                                      temp_chunk,
                                      append = TRUE)
              }
              disk.frame::delete(temp_df)
            })
            showNotification(
              ui = paste0("File ",
                          i,
                          " Imported Successfully"),
              duration = 3,
              type = "message"
            )
            action_import_tables$imported_tables <-
              action_import_tables$imported_tables + 1
          },
          error = function(err) {
            showNotification(
              ui = paste0(err, ". Data of this file not imported."),
              duration = 5,
              type = "error"
            )
          })
        }
        else if (info$import_type[[i]] == "Columns Selected in Table") {
          tryCatch({
            if (is.null(info$header_selected_columns[[i]]) ||
                identical(info$header_selected_columns[[i]], character(0))) {
              showNotification(
                ui = paste0(
                  "No columns in table selected for file ",
                  i,
                  ". Data of this file not imported."
                ),
                duration = 5,
                type = "error"
              )
            }
            else{
              withProgress(message = "Import in Progress", expr =  {
                temp_df <-
                  disk.frame::csv_to_disk.frame(
                    infile = info$file_paths[[i]],
                    sep = info$delimiter,
                    select = info$header_selected_columns[[i]],
                    header = info$column_names_present[[i]]
                  )
                chunk_ids <- disk.frame::get_chunk_ids(temp_df)
                for (chunk in chunk_ids) {
                  temp_chunk <- disk.frame::get_chunk(temp_df, as.numeric(chunk))
                  # Never overwrite data, always append.
                  RSQLite::dbWriteTable(conn$active_db,
                                        info$table_names[[i]],
                                        temp_chunk,
                                        append = TRUE)
                }
                disk.frame::delete(temp_df)
              })
              showNotification(
                ui = paste0("File ",
                            i,
                            " Imported Successfully"),
                duration = 3,
                type = "message"
              )
              action_import_tables$imported_tables <-
                action_import_tables$imported_tables + 1
            }
          },
          error = function(err) {
            showNotification(
              ui = paste0(err, ". Data of this file not imported."),
              duration = 5,
              type = "error"
            )
          })
        }
        else if (info$import_type[[i]] == "Columns from List") {
          tryCatch({
            if (is.null(info$checkbox_selected_columns[[i]]) ||
                identical(info$checkbox_selected_columns[[i]], character(0))) {
              showNotification(
                ui = paste0(
                  "No columns in list selected for file ",
                  i,
                  ". Data of this file not imported."
                ),
                duration = 5,
                type = "error"
              )
            }
            else{
              withProgress(message = "Import in Progress", expr =  {
                temp_df <-
                  disk.frame::csv_to_disk.frame(
                    infile = info$file_paths[[i]],
                    sep = info$delimiter,
                    select = info$checkbox_selected_columns[[i]],
                    header = info$column_names_present[[i]]
                  )
                chunk_ids <- disk.frame::get_chunk_ids(temp_df)
                for (chunk in chunk_ids) {
                  temp_chunk <- disk.frame::get_chunk(temp_df, as.numeric(chunk))
                  # Never overwrite data, always append.
                  RSQLite::dbWriteTable(conn$active_db,
                                        info$table_names[[i]],
                                        temp_chunk,
                                        append = TRUE)
                }
                disk.frame::delete(temp_df)
              })
              showNotification(
                ui = paste0("File ",
                            i,
                            " Imported Successfully"),
                duration = 3,
                type = "message"
              )
              action_import_tables$imported_tables <-
                action_import_tables$imported_tables + 1
            }
          },
          error = function(err) {
            showNotification(
              ui = paste0(err, ". Data of this file not imported."),
              duration = 5,
              type = "error"
            )
          })
        }
        else if (info$import_type[[i]] == "Columns by Name") {
          tryCatch({
            if (is.null(info$specify_columns[[i]]) ||
                identical(info$specify_columns[[i]], character(0)) ||
                info$specify_columns[[i]] == "") {
              showNotification(
                ui = paste0(
                  "No columns specified for file ",
                  i,
                  ". Data of this file not imported."
                ),
                duration = 5,
                type = "error"
              )
            }
            else{
              withProgress(message = "Import in Progress", expr =  {
                col_names <- unlist(strsplit(input$specify_columns, "\\,\\s*"))
                temp_df <-
                  disk.frame::csv_to_disk.frame(
                    infile = info$file_paths[[i]],
                    sep = info$delimiter,
                    header = info$column_names_present[[i]],
                    select = col_names
                  )
                chunk_ids <- disk.frame::get_chunk_ids(temp_df)
                for (chunk in chunk_ids) {
                  temp_chunk <- disk.frame::get_chunk(temp_df, as.numeric(chunk))
                  # Never overwrite data, always append.
                  RSQLite::dbWriteTable(conn$active_db,
                                        info$table_names[[i]],
                                        temp_chunk,
                                        append = TRUE)
                }
                disk.frame::delete(temp_df)
              })
              showNotification(
                ui = paste0("File ",
                            i,
                            " Imported Successfully"),
                duration = 3,
                type = "message"
              )
              action_import_tables$imported_tables <-
                action_import_tables$imported_tables + 1
            }
          },
          error = function(err) {
            showNotification(
              ui = paste0(err, ". Data of this file not imported."),
              duration = 5,
              type = "error"
            )
          },
          warning = function(war) {
            showNotification(ui = toString(war),
                             duration = 5,
                             type = "error")
          })
        }
      }
    }
  })
  
  return(action_import_tables)
}

## To be copied in the UI
# mod_import_tables_ui("import_tables_ui_1")

## To be copied in the server
# callModule(mod_import_tables_server, "import_tables_ui_1")

