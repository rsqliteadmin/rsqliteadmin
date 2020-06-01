## Functions for module create_databases

# Returns the list of databases in folder "Databases"
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
