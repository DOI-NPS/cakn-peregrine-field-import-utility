library(odbc)
library(DBI)
library(RSQLite)
library(tidyverse)
library(here)
library(diffdf)

source("db_tools.R")
source("db_update.R")
source("db_insert.R")

SRC_CONN_STR <- "c:/file/path/source_db.gpkg"
TGT_CONN_STR <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=target_db.accdb"

CORRECTIONS_CSV <- "c:/fake/path/corrections.csv"
LOG_DIR <- "c:/fake/dir_path"

main <- function() {
  src_conn <- get_sqlite_connection(SRC_CONN_STR)
  tgt_conn <- get_access_connection(TGT_CONN_STR)

  ## Guarantees that db connections are always closed, regardless of
  ## error or program stoppage.
  on.exit({
    DBI::dbDisconnect(src_conn)
    DBI::dbDisconnect(tgt_conn)
  }, add = TRUE)

  repeat {
    cat("Options:",
        "l  - find lookup table diffs and print to screen",
        "lw - find lookup table diffs and write to file",
        "ac - append data from source to target db WITH corrections file.",
        "aw - append data from source to target db WITHOUT corrections file.",
        "q  - quit this program\n",
        sep = "\n")

    cat("Type an option and press the 'Enter' key.\n")

    opt <- readline("Enter an option: ")

    if (opt == 'l') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_lu_diffs(src_conn, tgt_conn)
      print_diffs(diffs)
      cat("\nDone!\n\n")
    } else if (opt == 'lw') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_lu_diffs(src_conn, tgt_conn)
      if (write_diffs(diffs, LOG_DIR)) {
          cat("\nDone!\n\n")
      } else {
          cat("\n*****\nAn ERROR occured when writing log file.\nIs the LOG directory path VALID?\n*****\n\n")
      }
    } else if (opt == 'ac') {
      if (file.exists(CORRECTIONS_CSV)) {
        cat("Appending data...\n")
        append_peregrine_data(src_conn, tgt_conn, CORRECTIONS_CSV)
        cat("\nDone!\n\n")
      } else {
        cat("\nCan't find corrections file. Fix file path.\n")
      }
    } else if (opt == 'aw') {
      cat("Appending data...\n")
      append_peregrine_data(src_conn, tgt_conn)
      cat("\nDone!\n\n")
    } else if (opt == 'q') {
      cat("Exiting...\n")
      break
    } else {
      cat("*** Must choose an option ***\n")
    }
  }
}
