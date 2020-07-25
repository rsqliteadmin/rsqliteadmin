#' import_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_table_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Import Table",
    br(),
    fluidRow(p(h2(
      strong("Import a new Table")
    ))),
    fluidRow(
      column(
        width = 1,
        shinyFiles::shinyFilesButton(
          id = ns("file_name"),
          label = "Select File",
          title = "Select File",
          multiple = FALSE,
          style = 'padding:9px; font-size:110%'
        )
      ),
      column(width = 6,
             verbatimTextOutput(ns("file_selected")))
    ),
    fluidRow(
      conditionalPanel(
        condition = paste0("input['", ns("specify_custom_separator"), "'] == false"),
        column(
          width = 2,
          selectInput(
            inputId = ns("separator"),
            label = "Separator",
            choices = c(",", "TAB", ";")
          )
        )
      ),
      column(
        width = 2,
        div(
          style = "display: inline-block; margin-top:12%",
          checkboxInput(
            inputId = ns("specify_custom_separator"),
            label = "Specify Custom Separator"
          )
        )
      ),
      column(
        width = 8,
        conditionalPanel(
          condition = paste0("input['", ns("specify_custom_separator"), "'] == true"),
          column(
            width = 4,
            textInput(
              inputId = ns("custom_separator"),
              label = "Custom Separator",
              value = ","
            )
          )
        ),
        column(width = 4,
               actionButton(
                 inputId = ns("update_header"),
                 label = "Update Header"
               ))
      )
    ),
    fluidRow(column(
      width = 4,
      textInput(
        inputId = ns("table_name"),
        label = h4(strong("Table Name")),
        placeholder = "Enter Table Name"
      )
    )),
    column(width = 3,
           checkboxInput(ns(
             "import_selected_columns"
           ),
           label = "Import Selected Columns")),
    uiOutput(ns("display_header_ui")),
    fluidRow(
      column(width = 2,
             checkboxInput(
               inputId = ns("overwrite"),
               label = "Overwrite Table"
             )),
      column(
        width = 3,
        checkboxInput(inputId = ns("first_row"),
                      label = "First Row contains Column Names")
      ),
      column(
        width = 4,
        checkboxInput(inputId = ns("trim_ws"),
                      label = "Trim Whitespaces")
      )
    ),
    # column(
    #   width = 4,
    #   textInput(inputId = ns("na_values"),
    #             label = "Strings to be treated as NA values.",
    #             value = "'', 'NA'")
    # )),
    fluidRow(
      actionButton(inputId = ns("import"),
                   label = "Import"),
      actionButton(ns("import_multiple_tables"), label = "Import Multiple Tables")
    )
  )
}

#' import_table Server Function
#'
#' @noRd
mod_import_table_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(file_path = NULL,
                         header_data = NULL)
  
  action_import_table <- reactiveValues(imported_table = NULL,
                                        imported_multiple_tables = NULL)
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "file_name",
                              roots = roots)
  
  observeEvent(input$file_name, {
    tryCatch({
      path <-
        shinyFiles::parseFilePaths(roots = roots, input$file_name)
      file_path <- path$datapath
      if (!(identical(file_path, character(0))))
      {
        info$file_path <- file_path
      }
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". Data not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "files_multiple_table",
                              roots = roots)
  
  observeEvent(input$import_multiple_tables, {
    showModal(
      modalDialog(
        size = "l",
        textInput(inputId = ns("extension"),
                  label = "Enter Extension"),
        textInput(
          inputId = ns("separator_multiple_tables"),
          label = "Enter Separator"
        ),
        shinyFiles::shinyFilesButton(
          id = ns("files_multiple_table"),
          label = "Select File",
          title = "Select File",
          multiple = TRUE
        ),
        actionButton(
          inputId = ns("confirm_multiple_tables"),
          label = "Confirm"
        )
      )
    )
  })
  
  observeEvent(input$confirm_multiple_tables, {
    tryCatch({
      f <- function(conn, table_name)
      {
        function(x, pos)
        {
          RSQLite::dbWriteTable(conn,
                                table_name,
                                x,
                                append = TRUE)
        }
      }
      paths <-
        shinyFiles::parseFilePaths(roots = roots, input$files_multiple_table)
      print(dim(paths)[1])
      if (dim(paths)[1] == 0)
        showNotification(
          ui =  paste0("Please select some files first."),
          duration = 5,
          type = "error"
        )
      else if(input$separator_multiple_tables=="")
        showNotification(
          ui =  paste0("Please enter a separator."),
          duration = 5,
          type = "error"
        )
      else{
        for (i in seq_len(dim(paths)[1])) {
          file_path <- paths$datapath[i]
          print(file_path)
          table_name  <-
            tools::file_path_sans_ext(basename(file_path))
          print(table_name)
          library(readr)
          if (isTRUE(grepl("\\", input$separator_multiple_tables, fixed = TRUE)))
          {
            readr::read_delim_chunked(
              file = file_path,
              delim = eval(parse(
                text = sub(
                  "\\",
                  "",
                  deparse(input$separator_multiple_tables),
                  fixed = TRUE
                )
              )),
              callback = DataFrameCallback$new(f(conn$active_db,
                                                 table_name))
            )
          }
          else
          {
            readr::read_delim_chunked(
              file = file_path,
              delim = input$separator_multiple_tables,
              callback = DataFrameCallback$new(f(conn$active_db,
                                                 table_name))
            )
          }
        }
        action_import_table$imported_multiple_tables <-
          input$import_multiple_tables
        showNotification(
          ui =  paste0("Selected tables imported successfully."),
          duration = 5,
          type = "message"
        )
      }
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". Data not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  observeEvent(info$file_path, {
    if (!is.null(info$file_path)) {
      tryCatch({
        updateTextInput(
          session = session,
          inputId = "table_name",
          value = tools::file_path_sans_ext(basename(info$file_path))
        )
        if (isTRUE(input$specify_custom_separator) &&
            input$custom_separator == "")
          showNotification(
            ui =  paste0("Please enter a separator."),
            duration = 3,
            type = "error"
          )
        else if (isTRUE(input$specify_custom_separator)) {
          if (isTRUE(grepl("\\", input$custom_separator, fixed = TRUE)))
            info$header_data <- readr::read_delim(
              file = info$file_path,
              delim = eval(parse(
                text = sub("\\", "", deparse(input$custom_separator), fixed = TRUE)
              )),
              n_max = 5
            )
          else
            info$header_data <- readr::read_delim(
              file = info$file_path,
              delim = input$custom_separator,
              n_max = 5
            )
        }
        else if (input$separator == "TAB") {
          info$header_data <- readr::read_delim(
            file = info$file_path,
            delim = "\t",
            n_max = 5
          )
        }
        else {
          info$header_data <- readr::read_delim(
            file = info$file_path,
            delim = input$separator,
            n_max = 5
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
  
  output$display_header_ui <- renderUI({
    fluidRow(conditionalPanel(
      condition = !is.null(info$file_path),
      column(width = 12,
             DT::DTOutput(ns(
               "display_header"
             )),
             style = "overflow-y: scroll;overflow-x: scroll;")
    ))
  })
  
  output$display_header <- DT::renderDT(expr = {
    DT::datatable(data = info$header_data,
                  selection = list(target = "column"),
                  options = list(dom = 't'))
  })
  
  observeEvent(input$update_header, {
    if (!is.null(info$file_path)) {
      tryCatch({
        if (isTRUE(input$specify_custom_separator) &&
            input$custom_separator == "")
          showNotification(
            ui =  paste0("Please enter a separator."),
            duration = 3,
            type = "error"
          )
        else if (isTRUE(input$specify_custom_separator)) {
          if (isTRUE(grepl("\\", input$custom_separator, fixed = TRUE)))
            info$header_data <- readr::read_delim(
              file = info$file_path,
              delim = eval(parse(
                text = sub("\\", "", deparse(input$custom_separator), fixed = TRUE)
              )),
              n_max = 5
            )
          else
            info$header_data <- readr::read_delim(
              file = info$file_path,
              delim = input$custom_separator,
              n_max = 5
            )
        }
        else if (input$separator == "TAB") {
          info$header_data <- readr::read_delim(
            file = info$file_path,
            delim = "\t",
            n_max = 5
          )
        }
        else {
          info$header_data <- readr::read_delim(
            file = info$file_path,
            delim = input$separator,
            n_max = 5
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
  
  # Reference Here: https://stackoverflow.com/questions/43677277/reading-csv-files-in-chunks-with-readrread-csv-chunked
  # Reference Here: https://stackoverflow.com/a/49241426/
  
  observeEvent(input$import,
               {
                 f <- function(conn, table_name, overwrite, na)
                 {
                   function(x, pos)
                   {
                     RSQLite::dbWriteTable(conn,
                                           table_name,
                                           x,
                                           append = TRUE,
                                           overwrite = overwrite)
                   }
                 }
                 tryCatch({
                   if (isTRUE(input$import_selected_columns))
                   {
                     if (is.null(input$display_header_columns_selected))
                       showNotification(
                         ui =  paste0(
                           "No column selected.
                         Please select a column
                         or uncheck \"Import Selected Columns\""
                         ),
                         duration = 5,
                         type = "error"
                       )
                     else
                       if (input$table_name == "")
                         showNotification(
                           ui =  paste0("Please enter a table name."),
                           duration = 3,
                           type = "error"
                         )
                     else
                       if (is.null(info$file_path))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                     else
                       if (isTRUE(input$specify_custom_separator) &&
                           input$custom_separator == "")
                         showNotification(
                           ui =  paste0("Please enter a separator."),
                           duration = 3,
                           type = "error"
                         )
                     else
                       if (isTRUE(input$specify_custom_separator))
                       {
                         library(readr)
                         col_names <-
                           colnames(info$header_data)[input$display_header_columns_selected]
                         col_names_list = list()
                         for (i in col_names)
                         {
                           col_names_list[[i]] = "?"
                         }
                         if (isTRUE(grepl("\\", input$custom_separator, fixed = TRUE)))
                         {
                           readr::read_delim_chunked(
                             file = info$file_path,
                             delim = eval(parse(
                               text = sub("\\", "", deparse(input$custom_separator), fixed = TRUE)
                             )),
                             col_types = do.call(cols_only, col_names_list),
                             callback = DataFrameCallback$new(
                               f(
                                 conn$active_db,
                                 input$table_name,
                                 input$overwrite
                               )
                             )
                           )
                         }
                         else
                         {
                           readr::read_delim_chunked(
                             file = info$file_path,
                             delim = input$custom_separator,
                             col_types = do.call(cols_only, col_names_list),
                             callback = DataFrameCallback$new(
                               f(
                                 conn$active_db,
                                 input$table_name,
                                 input$overwrite
                               )
                             )
                           )
                         }
                         action_import_table$imported_table <-
                           input$import
                       }
                     else
                       if (input$separator == "TAB")
                       {
                         library(readr)
                         col_names <-
                           colnames(info$header_data)[input$display_header_columns_selected]
                         col_names_list = list()
                         for (i in col_names)
                         {
                           col_names_list[[i]] = "?"
                         }
                         readr::read_delim_chunked(
                           file = info$file_path,
                           delim = "\t",
                           col_types = do.call(cols_only, col_names_list),
                           callback = DataFrameCallback$new(
                             f(conn$active_db,
                               input$table_name,
                               input$overwrite)
                           )
                         )
                         action_import_table$imported_table <-
                           input$import
                       }
                     else
                     {
                       library(readr)
                       col_names <-
                         colnames(info$header_data)[input$display_header_columns_selected]
                       col_names_list = list()
                       for (i in col_names)
                       {
                         col_names_list[[i]] = "?"
                       }
                       readr::read_delim_chunked(
                         file = info$file_path,
                         delim = input$separator,
                         col_types = do.call(cols_only, col_names_list),
                         callback = DataFrameCallback$new(
                           f(conn$active_db,
                             input$table_name,
                             input$overwrite)
                         )
                       )
                       action_import_table$imported_table <-
                         input$import
                     }
                   }
                   
                   else
                   {
                     if (input$table_name == "")
                       showNotification(
                         ui =  paste0("Please enter a table name."),
                         duration = 3,
                         type = "error"
                       )
                     else
                       if (is.null(info$file_path))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                     else
                       if (isTRUE(input$specify_custom_separator) &&
                           input$custom_separator == "")
                         showNotification(
                           ui =  paste0("Please enter a separator."),
                           duration = 3,
                           type = "error"
                         )
                     else
                       if (isTRUE(input$specify_custom_separator))
                       {
                         library(readr)
                         if (isTRUE(grepl("\\", input$custom_separator, fixed = TRUE)))
                         {
                           readr::read_delim_chunked(
                             file = info$file_path,
                             delim = eval(parse(
                               text = sub("\\", "", deparse(input$custom_separator), fixed = TRUE)
                             )),
                             trim_ws = input$trim_ws,
                             callback = DataFrameCallback$new(
                               f(
                                 conn$active_db,
                                 input$table_name,
                                 input$overwrite
                               )
                             )
                           )
                         }
                         else
                         {
                           readr::read_delim_chunked(
                             file = info$file_path,
                             delim = input$custom_separator,
                             trim_ws = input$trim_ws,
                             callback = DataFrameCallback$new(
                               f(
                                 conn$active_db,
                                 input$table_name,
                                 input$overwrite
                               )
                             )
                           )
                         }
                         action_import_table$imported_table <-
                           input$import
                       }
                     else
                       if (input$separator == "TAB")
                       {
                         library(readr)
                         readr::read_delim_chunked(
                           file = info$file_path,
                           delim = "\t",
                           trim_ws = input$trim_ws,
                           callback = DataFrameCallback$new(
                             f(conn$active_db,
                               input$table_name,
                               input$overwrite)
                           )
                         )
                         action_import_table$imported_table <-
                           input$import
                       }
                     else
                     {
                       library(readr)
                       readr::read_delim_chunked(
                         file = info$file_path,
                         delim = input$separator,
                         trim_ws = input$trim_ws,
                         callback = DataFrameCallback$new(
                           f(conn$active_db,
                             input$table_name,
                             input$overwrite)
                         )
                       )
                       action_import_table$imported_table <-
                         input$import
                     }
                   }
                 },
                 error = function(err)
                 {
                   showNotification(
                     ui =  paste0(err, ". Data not imported."),
                     duration = 5,
                     type = "error"
                   )
                 })
               })
  output$file_selected <- renderText({
    if (is.null(info$file_path))
      return("")
    else
      return(info$file_path)
  })
  
  return(action_import_table)
}

## To be copied in the UI
# mod_import_table_ui("import_table_ui_1")

## To be copied in the server
# callModule(mod_import_table_server, "import_table_ui_1")
