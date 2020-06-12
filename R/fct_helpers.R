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

## Functions for module view_tables

# Get active table column names
column_names_query <- function(active_table = NULL) {
  res <- paste0("SELECT name FROM PRAGMA_TABLE_INFO('",
                active_table,
                "');")
  return(res)
}

# Fetch data for a table
data_fetch_query <- function(active_table = NULL) {
  res <- paste0(
    "SELECT rowid AS row_id, ROW_NUMBER() OVER(ORDER BY rowid) AS row_number, * FROM ",
    active_table
  )
  return(res)
}

# Update data in a table
update_query <- function(active_table, column_name, value, rowid) {
  res <- NULL
  if (!is.na(as.numeric(value))) {
    res <-
      paste0("UPDATE ",
             active_table,
             " SET ",
             column_name,
             " = ",
             value,
             " WHERE rowid = ",
             rowid)
  }
  else{
    res <-
      paste0("UPDATE ",
             active_table,
             " SET ",
             column_name,
             " = '",
             value,
             "' WHERE rowid = ",
             rowid)
  }
  return(res)
}

# Delete a row from the table
delete_query <- function(active_table, rowid) {
  res <- paste0("DELETE FROM ",
                active_table,
                " WHERE rowid = ",
                rowid)
  print(res)
  return(res)
}

# Delete all rows from a table
delete_all_query <- function(active_table) {
  res <- paste0("DELETE FROM ",
                active_table)
  return(res)
}

# Insert data into a table
insert_query <- function(active_table, values) {
  insert_query_values <- "("
  i = 0
  for (val in values) {
    # Warning occurs here because of conversion into numeric type in if statement.
    i = i + 1
    if (!is.na(as.numeric(val))) {
      if (i != length(values))
        insert_query_values <-
          paste0(insert_query_values, val, ",")
      else
        insert_query_values <-
          paste0(insert_query_values, val, ")")
    }
    else{
      if (i != length(values))
        insert_query_values <-
          paste0(insert_query_values, '"', val, '",')
      else
        insert_query_values <-
          paste0(insert_query_values, '"', val, '")')
    }
  }
  res <-
    paste0("INSERT INTO ",
           active_table,
           " VALUES ",
           insert_query_values)
  return(res)
}