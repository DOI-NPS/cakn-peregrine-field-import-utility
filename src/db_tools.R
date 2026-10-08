
## Returns ODBC connection to database.
## - conn_string: valid ODBC connection string.
get_access_connection <- function(conn_string) {
  conn <- DBI::dbConnect(odbc::odbc(),
                         .connection_string = conn_string,
                         timezone = "America/Anchorage",
                         timezone_out = "America/Anchorage")
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

format_datetime_utc <- function(dt) {
  as.POSIXct(dt,
             "%Y-%m-%dT%H:%M:%S",
             tz="UTC")
}

format_date_ak  <- function(d) {
  as.POSIXct(d,
             "%Y-%m-%d",
             tz="America/Anchorage")
}

create_logger <- function() {
  env <- new.env()
  env$log <- ""
  return(env)
}

write_log <- function(log, dir_path) {
  file_path <- file.path(dir_path, paste0("append_log_", get_datetime_str(), ".txt"))
  writeLines(log, file_path)
}

get_datetime_str <- function() {
  return(format(Sys.time(), "%Y-%m-%dT%H.%M.%S"))
}

get_log_info <- function() {
  info <- paste0(
    "========================================================================================\n",
    "LOG INFO\n",
    "========================================================================================\n",
    "USER: ", Sys.info()[["user"]], "\n",
    "DATETIME: ", format(Sys.time(), "%B %d, %Y %H:%M:%S"), "\n\n")

  return(info)
}

copy_file <- function(from_path, to_path) {
  success <- file.copy(
    from = from_path,
    to = to_path,
    overwrite = TRUE)

  return(success)
}

inject_dt_into_fpath <- function(file_path) {
  dir_name <- dirname(file_path)
  file_name <- tools::file_path_sans_ext(basename(file_path))
  ext_type <- tools::file_ext(file_path)

  if (has_datetime(file_path)) {
    tgt_path <- sub("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}",
                    get_datetime_str(),
                    file_path)
  } else {
    tgt_path <- paste0(dir_name, "/", file_name, "_", get_datetime_str(), ".", ext_type)
  }

  return(tgt_path)
}

has_datetime <- function(file_path) {
  exists <- grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", file_path)
  return(exists)
}
