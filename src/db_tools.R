

## Returns ODBC connection to database.
## - conn_string: valid ODBC connection string.
get_access_connection <- function(conn_string) {
  conn <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_string)
  return(conn)
}

get_sqlite_connection <- function(conn_string) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), conn_string)
  return(conn)
}

## Returns a data as a tibble, from table or saved query in database.
## Parameter(s):
##   conn: connection object to database.
##   tbl_name: code string identifying the table or query to be returned.
get_table <- function(conn, tbl_name) {
  tbl <- DBI::dbReadTable(conn, tbl_name)
  names(tbl) <- tolower(names(tbl))

  return(as_tibble(tbl))
}

## Returns a data as a tibble, from query statement on database.
## Parameter(s):
##   conn: connection object to database.
##   query_str: query string statement of data to be returned.
get_query <- function(conn, query_str) {
  qry <- DBI::dbGetQuery(conn, query_str)

  names(qry) <- tolower(names(qry))

  return(as_tibble(qry))
}
