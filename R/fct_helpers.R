## Functions for module create_databases

# Returns the list of databases in folder "Databases"
db_list <- function() {
  return(list.files(path = "./Databases", pattern = ".db$"))
}

# Creates a new database
create_db <- function(name = NULL) {
  db_name <- paste0("./Databases/", name, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  RSQLite::dbDisconnect(conn)
}