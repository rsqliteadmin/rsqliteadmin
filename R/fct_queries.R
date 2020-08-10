# Note: For SQLite, A keyword in single quotes ('') is a string literal
#               and A keyword in double-quotes ("") is an identifier.

## Functions for module view_tables

# Get active table column names
column_names_query <- function(active_table = NULL) {
  res <- paste0("SELECT name FROM PRAGMA_TABLE_INFO('",
                active_table,
                "');")
  return(res)
}

#Get number of rows for a table.
total_rows_query <- function(active_table = NULL) {
  res <- paste0('SELECT COUNT(*) FROM "', active_table, '"')
  return(res)
}

# Fetch data for a table
data_fetch_query <- function(active_table = NULL,
                             number_rows = NULL,
                             offset = NULL) {
  res <- paste0(
    'SELECT rowid AS row_id, * FROM "',
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

# Rename an existing column

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
  return(res)
}

# Add column to an existing table

add_column_query <- function(active_table = NULL,
                             column_details_query = NULL) {
  res <- paste0('ALTER TABLE "',
                active_table,
                '" ADD COLUMN ',
                column_details_query)
  return(res)
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

## Functions for module search

search_query_sqlite <- function(display_columns = NULL,
                                search_columns = NULL,
                                table_name = NULL,
                                search_string = NULL,
                                escape_characters = NULL) {
  if (isTRUE(escape_characters)) {
    res <- "SELECT "
    for (i in display_columns) {
      res <- paste0(res, "\"", i, "\", ")
    }
    # Remove the last comma.
    res <- substr(res, 1, nchar(res) - 2)
    res <- paste0(res, " FROM \"", table_name, "\" WHERE ")
    for (i in display_columns) {
      res <- paste0(res, "\"", i, "\" LIKE '", search_string,
                    "' ESCAPE '\\' OR ")
    }
    # Remove the last OR.
    res <- substr(res, 1, nchar(res) - 15)
    res <- paste0(res, ";")
  }
  else {
    res <- "SELECT "
    for (i in display_columns) {
      res <- paste0(res, "\"", i, "\", ")
    }
    # Remove the last comma.
    res <- substr(res, 1, nchar(res) - 2)
    res <- paste0(res, " FROM \"", table_name, "\" WHERE ")
    for (i in display_columns) {
      res <- paste0(res, "\"", i, "\" LIKE '", search_string, "' OR ")
    }
    # Remove the last OR.
    res <- substr(res, 1, nchar(res) - 4)
    res <- paste0(res, ";")
  }
  return(res)
}

search_query_unix <- function(display_columns = NULL,
                              search_columns = NULL,
                              table_name = NULL,
                              search_string = NULL) {
  res <- "SELECT "
  for (i in display_columns) {
    res <- paste0(res, "\"", i, "\", ")
  }
  # Remove the last comma.
  res <- substr(res, 1, nchar(res) - 2)
  res <- paste0(res, " FROM \"", table_name, "\" WHERE ")
  for (i in display_columns) {
    res <- paste0(res, "\"", i, "\" GLOB '", search_string, "' OR ")
  }
  # Remove the last OR.
  res <- substr(res, 1, nchar(res) - 4)
  res <- paste0(res, ";")
  return(res)
}

search_query_regex <- function(display_columns = NULL,
                              search_columns = NULL,
                              table_name = NULL,
                              search_string = NULL) {
  res <- "SELECT "
  for (i in display_columns) {
    res <- paste0(res, "\"", i, "\", ")
  }
  # Remove the last comma.
  res <- substr(res, 1, nchar(res) - 2)
  res <- paste0(res, " FROM \"", table_name, "\" WHERE ")
  for (i in display_columns) {
    res <- paste0(res, "\"", i, "\" REGEXP '", search_string, "' OR ")
  }
  # Remove the last OR.
  res <- substr(res, 1, nchar(res) - 4)
  res <- paste0(res, ";")
  return(res)
}

## Functions for module clone_tables

clone_query <- function(new_table_name = NULL,
                        old_table_name = NULL,
                        column_list = NULL,
                        include_data = NULL) {
  res <- paste0("CREATE TABLE \"",
                new_table_name,
                "\" AS SELECT ")
  for (i in column_list) {
    res <- paste0(res, "\"", i, "\", ")
  }
  # Remove the last comma.
  res <- substr(res, 1, nchar(res) - 2)
  res <- paste0(res, " FROM \"",
                old_table_name,
                "\";")
  # If data isn't to be included, 
  # we mention an always false condition.
  if(!isTRUE(include_data)){
    res <- substr(res, 1, nchar(res) - 1)
    res <- paste0(res, " WHERE 1==0;")
  }
    print(res)
    return(res)
}

## Functions for module query

recent_query <- function(query = NULL,
                         db_name = NULL) {
  res <- paste0("INSERT INTO \"history\"(\"Query\", \"Database\") VALUES('",
                query, 
                "','", 
                db_name, 
                "');")
  print(res)
  return(res)
}

recent_data_fetch_query <- function() {
  res <- "SELECT rowid AS row_id, * FROM \"history\" ORDER BY \"id\" DESC;"
  return(res)
}
