library(odbc)
library(DBI)
library(tidyverse)
library(here)

source("db_tools.R")
source("db_update.R")
source("db_insert.R")

main <- function() {
  db_conn_str <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=target_db.accdb"
  gpkg_conn_str <- "c:/file/path/source_db.gpkg"

  db_conn <- get_access_connection(db_conn_str)
  gpkg_conn <- get_sqlite_connection(gpkg_conn_str)

  update_target(db_conn)
  append_peregrine_data(db_conn, gpkg_conn)

  DBI::dbDisconnect(db_conn)
  DBI::dbDisconnect(gpkg_conn)
}
