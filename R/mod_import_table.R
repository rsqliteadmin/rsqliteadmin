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
          id = ns("file_names"),
          label = "Select File(s)",
          title = "Select File(s)",
          multiple = TRUE,
          style = 'padding:9px; font-size:110%'
        )
      ),
      column(width = 6,
             verbatimTextOutput(ns("file_selected")))
    ),
    fluidRow(column(
      width = 4,
      textInput(inputId = ns("separator"),
                label = "Separator")
    )),
    fluidRow(column(
      width = 4,
      textInput(
        inputId = ns("table_name"),
        label = h4(strong("Table Name")),
        placeholder = "Enter Table Name"
      )
    )),
    uiOutput(ns("display_header_ui")),
    fluidRow(
      column(width = 3,
             radioButtons(ns(
               "import_type"
             ),
             label = "What method do you want to import with?",
             choices = c("Import All Columns",
                         "Import Selected Columns in Table",
                         "Import Columns Selected via Checkboxes",
                         "Import Specified Columns"))),
    ),
    fluidRow(
      
      actionButton(inputId = ns("select_columns"),
                   label = "Select Columns to Import"),
      actionButton(inputId = ns("specify_columns"),
                   label = "Specify Columns to Import"),
      actionButton(inputId = ns("import"),
                   label = "Import")
    )
  )
}

#' import_table Server Function
#'
#' @noRd
mod_import_table_server <- function(input, output, session, conn) {
  ns <- session$ns
  
  info <- reactiveValues(file_paths = NULL,
                         header_data = NULL,
                         delimiter = NULL)
  
  action_import_table <- reactiveValues(imported_table = NULL,
                                        imported_multiple_tables = NULL)
  
  roots = c(
    shinyFiles::getVolumes()(),
    "Current Working Directory" = '.',
    "Home" = fs::path_home()
  )
  
  shinyFiles::shinyFileChoose(input = input,
                              id = "file_names",
                              roots = roots)
  
  observeEvent(input$select_columns, {
    if (!is.null(info$file_paths)) {
      showModal(modalDialog(
        size = "l",
        checkboxGroupInput(
          inputId = ns("selected_columns"),
          label = "Select columns to import",
          choices = colnames(info$header_data),
          selected = input$selected_columns
        ),
        actionButton(
          inputId = ns("confirm_select_columns"),
          label = "Confirm"
        ),
        actionButton(inputId = ns("select_all"),
                     label = "Select/Deselect All")
      ))
    }
  })
  
  observeEvent(input$confirm_select_columns, {
    removeModal()
    print(input$selected_columns)
  })
  
  observeEvent(input$specify_columns, {
    if (!is.null(info$file_paths)) {
      showModal(modalDialog(
        size = "l",
        textInput(inputId = ns("specified_columns"),
                  label = "Specify Column Names separated by \",\"",
                  value = input$specified_columns),
        actionButton(
          inputId = ns("confirm_specify_columns"),
          label = "Confirm"
        )
      ))
    }
  })
  
  observeEvent(input$confirm_specify_columns, {
    removeModal()
  })
  
  observeEvent(input$select_all, {
    if (input$select_all %% 2 == 0)
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_columns",
        choices = colnames(info$header_data)
      )
    else
      updateCheckboxGroupInput(
        session = session,
        inputId = "selected_columns",
        choices = colnames(info$header_data),
        selected = colnames(info$header_data)
      )
  })
  
  observeEvent(input$file_names, {
    tryCatch({
      paths <-
        shinyFiles::parseFilePaths(roots = roots, input$file_names)
      if (dim(paths)[1] != 0)
        info$file_paths <- paths
    },
    error = function(err) {
      showNotification(
        ui =  paste0(err, ". Data not imported."),
        duration = 5,
        type = "error"
      )
    })
  })
  
  observeEvent(info$file_paths, {
    if (!is.null(info$file_paths)) {
      tryCatch({
        if (dim(info$file_paths)[1] != 0) {
          updateTextInput(
            session = session,
            inputId = "table_name",
            value = tools::file_path_sans_ext(basename(info$file_paths$datapath[1]))
          )
          info$delimiter <-
            reader::get.delim(fn = info$file_paths$datapath[1])
          if (identical(info$delimiter, "\t"))
            info$delimiter <- "\\t"
          else if (identical(info$delimiter, "\t| +"))
            info$delimiter <- "\\t| +"
          updateTextInput(
            session = session,
            inputId = "separator",
            value = info$delimiter
          )
          info$header_data <- readr::read_delim(
            file = info$file_paths$datapath[1],
            delim = info$delimiter,
            n_max = 5
          )
        }
        if (dim(info$file_paths)[1] > 1)
          updateRadioButtons(session = session, 
                             inputId = "import_type",
                             choices = c("Import All Columns"))
        else
          updateRadioButtons(session = session, 
                             inputId = "import_type",
                             choices = c("Import All Columns",
                                         "Import Selected Columns in Table",
                                         "Import Columns Selected via Checkboxes",
                                         "Import Specified Columns"))
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
      condition = !is.null(info$file_paths),
      column(width = 12,
             DT::DTOutput(ns(
               "display_header"
             )),
             style = "overflow-y: scroll;overflow-x: scroll;")
    ))
  })
  
  output$display_header <- DT::renderDT(expr = {
    DT::datatable(
      data = info$header_data,
      selection = list(target = "column"),
      plugins = "ellipsis",
      options = list(dom = 't',
                     columnDefs = list(list(
                       targets = "_all",
                       render = DT::JS("$.fn.dataTable.render.ellipsis(20)"))))
    )
  })
  
  observeEvent(input$separator, {
    tryCatch({
      if (isTRUE(grepl("\\", input$separator, fixed = TRUE)))
        info$delimiter <- eval(parse(text = sub(
          "\\", "", deparse(input$separator), fixed = TRUE
        )))
      else
        info$delimiter <- input$separator
      info$header_data <- readr::read_delim(
        file = info$file_paths$datapath[1],
        delim = info$delimiter,
        n_max = 5
      )
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
  
  # Reference Here: https://stackoverflow.com/questions/43677277/reading-csv-files-in-chunks-with-readrread-csv-chunked
  # Reference Here: https://stackoverflow.com/a/49241426/
  
  observeEvent(input$import,
               {
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
                 tryCatch({
                   if (identical(input$import_type, "Import Selected Columns in Table"))
                   {
                     if (is.null(input$display_header_columns_selected))
                       showNotification(
                         ui = "No column selected.",
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
                       if (is.null(info$file_paths))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                     # else if (dim(info$file_paths)[1] > 1)
                     #   showNotification(
                     #     ui =  paste0(
                     #       "Uncheck \"Import Selected Columns\" to  import multiple files."
                     #     ),
                     #     duration = 3,
                     #     type = "error"
                     #   )
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
                       withProgress(message = "Import in Progress", expr =  {
                         readr::read_delim_chunked(
                           file = info$file_paths$datapath[1],
                           delim = info$delimiter,
                           col_types = do.call(cols_only, col_names_list),
                           callback = DataFrameCallback$new(f(
                             conn$active_db,
                             input$table_name
                           ))
                         )
                       })
                       showNotification(
                         ui = "Table Imported Successfully",
                         duration = 3,
                         type = "message"
                       )
                       action_import_table$imported_table <-
                         input$import
                     }
                   }
                   
                   else if(identical(input$import_type, "Import All Columns"))
                   {
                     if (dim(info$file_paths)[1] == 1) {
                       if (input$table_name == "")
                         showNotification(
                           ui =  paste0("Please enter a table name."),
                           duration = 3,
                           type = "error"
                         )
                       else if (is.null(info$file_paths))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                       else{
                         library(readr)
                         print(info$file_paths$datapath[1])
                         withProgress(message = "Import in Progress", expr =  {
                           readr::read_delim_chunked(
                             file = info$file_paths$datapath[1],
                             delim = info$delimiter,
                             callback = DataFrameCallback$new(f(
                               conn$active_db,
                               input$table_name
                             ))
                           )
                         })
                         showNotification(
                           ui = "Table Imported Successfully",
                           duration = 3,
                           type = "message"
                         )
                         action_import_table$imported_table <-
                           input$import
                       }
                     }
                     else{
                       if (is.null(info$file_paths))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                       else{
                         for (i in seq_len(dim(info$file_paths)[1])) {
                           library(readr)
                           table_name <-
                             tools::file_path_sans_ext(basename(info$file_paths$datapath[i]))
                           withProgress(message = "Import in Progress", expr =  {
                             readr::read_delim_chunked(
                               file = info$file_paths$datapath[i],
                               delim = info$delimiter,
                               callback = DataFrameCallback$new(f(
                                 conn$active_db,
                                 table_name
                               ))
                             )
                           })
                         }
                         showNotification(
                           ui = "Tables Imported Successfully",
                           duration = 3,
                           type = "message"
                         )
                       }
                       action_import_table$imported_table <-
                         input$import
                     }
                   }
                   else if(identical(input$import_type, "Import Columns Selected via Checkboxes"))
                   {
                     if (is.null(input$selected_columns))
                       showNotification(
                         ui =  "No Column Selected",
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
                       if (is.null(info$file_paths))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                     else
                     {
                       library(readr)
                       col_names <- input$selected_columns
                       col_names_list = list()
                       for (i in col_names)
                       {
                         col_names_list[[i]] = "?"
                       }
                       withProgress(message = "Import in Progress", expr =  {
                       readr::read_delim_chunked(
                         file = info$file_paths$datapath[1],
                         delim = info$delimiter,
                         col_types = do.call(cols_only, col_names_list),
                         callback = DataFrameCallback$new(f(
                           conn$active_db,
                           input$table_name
                         ))
                       )
                       })
                       showNotification(
                         ui = "Table Imported Successfully",
                         duration = 3,
                         type = "message"
                       )
                       action_import_table$imported_table <-
                         input$import
                     }
                   }
                   else if(identical(input$import_type, "Import Specified Columns")){
                     if (input$specified_columns=="")
                       showNotification(
                         ui =  "No Column Specified",
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
                       if (is.null(info$file_paths))
                         showNotification(
                           ui =  paste0("No file selected."),
                           duration = 3,
                           type = "error"
                         )
                     else
                     {
                       library(readr)
                       col_names <- unlist(strsplit(input$specified_columns, "\\, |\\,"))
                       col_names_list = list()
                       for (i in col_names)
                       {
                         col_names_list[[i]] = "?"
                       }
                       withProgress(message = "Import in Progress", expr =  {
                       readr::read_delim_chunked(
                         file = info$file_paths$datapath[1],
                         delim = info$delimiter,
                         col_types = do.call(cols_only, col_names_list),
                         callback = DataFrameCallback$new(f(
                           conn$active_db,
                           input$table_name
                         ))
                       )
                       })
                       showNotification(
                         ui = "Table Imported Successfully",
                         duration = 3,
                         type = "message"
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
    if (is.null(info$file_paths))
      return("")
    else
    {
      res <- ""
      for (i in seq_len(dim(info$file_paths)[1]))
      {
        res <- paste0(res, info$file_paths$datapath[i], "\n")
      }
      return(res)
    }
  })
  
  return(action_import_table)
}

## To be copied in the UI
# mod_import_table_ui("import_table_ui_1")

## To be copied in the server
# callModule(mod_import_table_server, "import_table_ui_1")
