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

## Functions for module view_tables

# Get active table column names
column_names_query <- function(active_table = NULL) {
  res <- paste0("SELECT name FROM PRAGMA_TABLE_INFO('",
                active_table,
                "');")
  return(res)
}

#Get number of rows for a table.
total_rows_query <- function(active_table = NULL){
  res<- paste0('SELECT COUNT(*) FROM "', active_table, '"')
  return(res)
}

# Fetch data for a table
data_fetch_query <- function(active_table = NULL,
                             number_rows = NULL,
                             offset = NULL) {
  res <- paste0(
    'SELECT rowid AS row_id, ROW_NUMBER() OVER(ORDER BY rowid) AS row_number, * FROM "',
    active_table,
    '" LIMIT ',
    number_rows,
    ' OFFSET ',
    offset,
    ';'
  )
  return(res)
}

# Update data in a table
update_query <- function(active_table, column_name, value, rowid) {
  res <- NULL
  if (!is.na(as.numeric(value))) {
    res <-
      paste0('UPDATE "',
             active_table,
             '" SET "',
             column_name,
             '" = ',
             value,
             " WHERE rowid = ",
             rowid)
  }
  else{
    res <-
      paste0(
        'UPDATE "',
        active_table,
        '" SET "',
        column_name,
        '" = \'',
        value,
        '\' WHERE rowid = ',
        rowid
      )
  }
  return(res)
}

# Delete a row from the table
delete_query <- function(active_table, rowid) {
  res <- paste0('DELETE FROM "',
                active_table,
                '" WHERE rowid = ',
                rowid)
  print(res)
  return(res)
}

# Delete all rows from a table
delete_all_query <- function(active_table) {
  res <- paste0('DELETE FROM "',
                active_table, '"')
  return(res)
}

# Insert data into a table
insert_query <- function(active_table, values) {
  insert_query_values <- '('
  i = 0
  for (val in values) {
    # Warning occurs here because of conversion into numeric type in if statement.
    i = i + 1
    if (!is.na(as.numeric(val))) {
      if (i != length(values))
        insert_query_values <-
          paste0(insert_query_values, val, ',')
      else
        insert_query_values <-
          paste0(insert_query_values, val, ')')
    }
    else{
      if (i != length(values))
        insert_query_values <-
          paste0(insert_query_values, '\'', val, '\',')
      else
        insert_query_values <-
          paste0(insert_query_values, '\'', val, '\')')
    }
  }
  res <-
    paste0('INSERT INTO "',
           active_table,
           '" VALUES ',
           insert_query_values)
  return(res)
}

# Column definition query.

column_details_query <- function(column_name = NULL,
                                 data_type = NULL,
                                 primary_key = NULL,
                                 autoincrement_primary_key = NULL,
                                 sort_order_primary_key = NULL,
                                 on_conflict_primary_key = NULL,
                                 unique = NULL,
                                 on_conflict_unique = NULL,
                                 not_null = NULL,
                                 on_conflict_not_null = NULL,
                                 default = NULL,
                                 default_value_default = NULL,
                                 check_condition = NULL,
                                 specify_condition_check_condition = NULL,
                                 collate = NULL,
                                 collation_type_collate = NULL,
                                 foreign_key = NULL,
                                 foreign_table_foreign_key = NULL,
                                 foreign_column_foreign_key = NULL,
                                 on_update_foreign_key = NULL,
                                 on_delete_foreign_key = NULL,
                                 match_foreign_key = NULL,
                                 defer_first_foreign_key = NULL,
                                 defer_second_foreign_key = NULL) {
  res <- paste0('"', column_name, '" "', data_type, '" ')
  
  if (isTRUE(primary_key)) {
    res <- paste0(res, "PRIMARY KEY ")
    
    if (sort_order_primary_key != "")
      res <- paste0(res, sort_order_primary_key, " ")
    
    if (on_conflict_primary_key != "")
      res <-
        paste0(res, "ON CONFLICT ", on_conflict_primary_key, " ")
    
    if (isTRUE(autoincrement_primary_key))
      res <- paste0(res, "AUTOINCREMENT ")
    
  }
  
  if (isTRUE(unique)) {
    res <- paste0(res, "UNIQUE ")
    
    if (on_conflict_unique != "")
      res <- paste0(res, "ON CONFLICT ", on_conflict_unique, " ")
  }
  
  if (isTRUE(not_null)) {
    res <- paste0(res, "NOT NULL ")
    
    if (on_conflict_not_null != "")
      res <- paste0(res, "ON CONFLICT ", on_conflict_not_null, " ")
    
  }
  
  if (isTRUE(default)) {
    res <- paste0(res, "DEFAULT ")
    
    if (!is.na(as.numeric(default_value_default))) {
      res <- paste0(res, "(", default_value_default, ") ")
    }
    else{
      res <- paste0(res, "'", default_value_default, "' ")
    }
    
  }
  
  if (isTRUE(check_condition)) {
    res <-
      paste0(res, "CHECK (", specify_condition_check_condition, ") ")
    
  }
  
  if (isTRUE(collate)) {
    res <- paste0(res, "COLLATE ", collation_type_collate, " ")
    
  }
  
  if (isTRUE(foreign_key)) {
    res <- paste0(
      res,
      "REFERENCES \"",
      foreign_table_foreign_key,
      "\" (",
      foreign_column_foreign_key,
      ") "
    )
    
    if (on_update_foreign_key != "")
      res <- paste0(res, "ON UPDATE ", on_update_foreign_key, " ")
    
    if (on_delete_foreign_key != "")
      res <- paste0(res, "ON DELETE ", on_delete_foreign_key, " ")
    
    if (match_foreign_key != "")
      res <- paste0(res, "MATCH ", match_foreign_key, " ")
    
    if (defer_first_foreign_key != "")
      res <- paste0(res, defer_first_foreign_key, " ")
    
    if (defer_second_foreign_key != "")
      res <-
        paste0(res, "INITIALLY ", defer_second_foreign_key, " ")
    
  }
  
  return(res)
}

# Create a new table.

create_table_query <- function(table_name = NULL,
                               column_details_query = NULL) {
  res <- paste0("CREATE TABLE \"", table_name, "\" ( ")
  for (i in column_details_query) {
    res <- paste0(res, i, ", ")
  }
  # Remove the last comma.
  res <- substr(res, 1, nchar(res) - 2)
  res <- paste0(res, ");")
  return(res)
}

drop_table_query <- function(table_name = NULL) {
  res <- paste0("DROP TABLE \"", table_name, "\"")
  return(res)
}

# Rename an existing table.

rename_table_query <- function(old_name = NULL,
                               new_name = NULL) {
  res <-
    paste0('ALTER TABLE "', old_name, '" RENAME TO "', new_name, '";')
  return(res)
}

update_column_name_query <- function(table_name = NULL,
                                     old_name = NULL,
                                     new_name = NULL) {
  res <- paste0('ALTER TABLE "',
                table_name,
                '" RENAME COLUMN "',
                old_name, 
                '" TO "',
                new_name,
                '"')
  print(res)
  return(res)
}

# Add column to an existing table

add_column_query <- function(active_table = NULL,
                             column_details_query = NULL){
  res <- paste0('ALTER TABLE "',
                active_table,
                '" ADD COLUMN ',
                column_details_query)
  print(res)
  return(res)
}

## Functions for module side_panel

# Reference Here: https://stackoverflow.com/a/37595263
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

update_sidebar_db <- function(db_list){
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

update_sidebar_table <- function(input_sidebar_menu, active_db, db_list){
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