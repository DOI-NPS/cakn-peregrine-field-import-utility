library(odbc)
library(DBI)
library(RSQLite)
library(tidyverse)
library(here)

source("db_tools.R")
source("db_update.R")
source("db_insert.R")

main <- function() {
  scr_conn_str <- "c:/file/path/source_db.gpkg"
  tgt_conn_str <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=target_db.accdb"

  src_conn <- get_sqlite_connection(src_conn_str)
  tgt_conn <- get_access_connection(tgt_conn_str)

  update_target(tgt_conn)
  append_peregrine_data(src_conn, tgt_conn)

  DBI::dbDisconnect(src_conn)
  DBI::dbDisconnect(tgt_conn)
}
