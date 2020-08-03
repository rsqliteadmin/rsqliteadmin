# Note: For SQLite, A keyword in single quotes ('') is a string literal
#               and A keyword in double-quotes ("") is an identifier.

## Functions for module create_databases

# Returns the list of databases in folder "directory"
db_list <- function(directory = NULL) {
  if (is.null(directory))
    return(NULL)
  return(list.files(path = directory, pattern = ".db$"))
}

# Creates a new database
create_db <- function(name = NULL, directory = NULL) {
  db_name <- paste0(directory, name, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  RSQLite::dbDisconnect(conn)
}

## Functions for module dashboard_structure

# Reference Here: https://stackoverflow.com/a/37595263
convertMenuItem <- function(mi, tabName) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if (length(mi$attribs$class) > 0 &&
      mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  mi
}

# Update the database list without tables in the sidebar.
update_sidebar_db <- function(db_list) {
  db_menu <- list()
  for (i in seq_len(length(db_list))) {
    db_menu[[i]] <-
      convertMenuItem(
        shinydashboard::menuItem(
          text = db_list[i],
          tabName = paste0("db_", i),
          icon = icon("search", lib = "glyphicon")
        ),
        paste0("db_", i)
      )
  }
  return(db_menu)
}

# Update the database list with tables in the sidebar.
update_sidebar_table <-
  function(input_sidebar_menu, active_db, db_list) {
    selected_db_index <- strtoi(substr(
      input_sidebar_menu,
      start = 4,
      stop = nchar(input_sidebar_menu)
    ))
    selected_db <- db_list[selected_db_index]
    
    table_list <- RSQLite::dbListTables(active_db)
    
    db_menu <- list()
    for (i in seq_len(length(db_list))) {
      if (db_list[i] == selected_db &&
          !identical(table_list, character(0)))
      {
        db_menu[[i]] <-
          convertMenuItem(
            shinydashboard::menuItem(
              text = db_list[i],
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
              text = db_list[i],
              tabName = paste0("db_", i),
              icon = icon("search", lib = "glyphicon")
            ),
            paste0("db_", i)
          )
      }
    }
    
    return(db_menu)
  }

## Functions for module triggers

get_triggers_query <- function(active_table) {
  res <-
    paste0(
      "SELECT * FROM sqlite_master WHERE type = 'trigger' AND tbl_name = \"",
      active_table,
      "\";"
    )
  return(res)
}

create_trigger_query <-
  function(name,
           when,
           action,
           table_name,
           pre_condition,
           logic) {
    res <- paste0("CREATE TRIGGER \"",
                  name,
                  "\" ",
                  when,
                  " ",
                  action,
                  " ON \"",
                  table_name,
                  "\" FOR EACH ROW ")
    if (pre_condition != "") {
      res <- paste0(res, "WHEN ", pre_condition, " ")
    }
    
    res <- paste0(res,
                  "BEGIN ",
                  logic,
                  " END;")
    return(res)
  }

drop_trigger_query <- function(trigger_name) {
  res <- paste0("DROP TRIGGER \"",
                trigger_name,
                "\";")
}

table_structure_query <- function(table_name) {
  res <- paste0("pragma table_info('", table_name, "');")
  return(res)
}

## Functions for module export_data

export_data_fetch_query <- function(table_name = NULL,
                                    number_rows = NULL,
                                    offset = NULL,
                                    column_list = NULL) {
  res <- "SELECT "
  for (i in column_list) {
    res <- paste0(res, "\"", i, "\", ")
  }
  # Remove the last comma.
  res <- substr(res, 1, nchar(res) - 2)
  res <- paste0(res,
                ' FROM "',
                table_name,
                '" LIMIT ',
                number_rows,
                ' OFFSET ',
                offset,
                ';')
  return(res)
}

