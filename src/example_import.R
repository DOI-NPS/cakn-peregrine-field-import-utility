library(odbc)
library(DBI)
library(RSQLite)
library(tidyverse)
library(diffdf)
library(this.path)
library(tools)

source("db_tools.R")
source("db_update.R")
source("db_insert.R")
source("db_diff.R")

SRC_CONN_STR <- "c:/fake/path/source_db.gpkg"

TGT_DB_SRC_PATH <- "c:/fake/path/target_db.accdb"
NEW_TGT_DB_PATH <- inject_dt_into_fpath(TGT_DB_SRC_PATH)

TGT_CONN_STR <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", NEW_TGT_DB_PATH)

CORRECTIONS_CSV <- "c:/fake/path/corrections.csv"
LOG_DIR <- "c:/fake/dir_path"

main <- function() {
  ## Make a copy of the target source db, and work on that copy.
  if (! copy_file(TGT_DB_SRC_PATH, NEW_TGT_DB_PATH)) {
    stop("File copy failed: check paths exist and permissions are correct.")
  }

  src_conn <- get_sqlite_connection(SRC_CONN_STR)
  tgt_conn <- get_access_connection(TGT_CONN_STR)

  ## Guarantees that db connections are always closed, regardless of
  ## error or program stoppage.
  on.exit({
    DBI::dbDisconnect(src_conn)
    DBI::dbDisconnect(tgt_conn)
  }, add = TRUE)

  has_corr_file <- has_path(CORRECTIONS_CSV)

  if (is.null(has_corr_file)) {
      stop("The value of Variable 'CORRECTIONS_CSV' must be 'NULL' OR ",
           "a string of a valid file path.")
  }

  has_log_dir <- has_path(LOG_DIR)

  if (is.null(has_log_dir)) {
      stop("The value variable 'LOG_DIR' must be 'NULL' OR ",
           "a string of a valid directory path.")
  }

  logger <- create_logger()

  repeat {
    logger$log <- ""
    logger$log <- get_log_info()
    logger$log <- paste0(logger$log,
                         "File/directory paths: \n",
                         "Script name: ", basename(this.path()) , "\n",
                         "Script working dir: ", this.path::here() , "\n\n",
                         "Source geopackage: ", SRC_CONN_STR, "\n",
                         "Target db source: ", TGT_DB_SRC_PATH, "\n",
                         "Updated target db: ", NEW_TGT_DB_PATH, "\n",
                         "Corrections file: ", CORRECTIONS_CSV, "\n",
                         "LOG_DIR: ", LOG_DIR, "\n\n")

    cat("Options:",
        "lp  - print LOOKUP table diffs to screen AND write to a log file in LOG_DIR (if exists)",
        "lw  - print LOOKUP table diffs AND write to a log file in LOG_DIR (if exists)\n",
        "ew  - print survey_EVENT table diffs AND write to a log file in LOG_DIR (if exists)",
        "ewa - print survey_EVENT table, using alternate key, diffs AND",
        "      write to a log file in LOG_DIR (if exists)\n",
        "ow  - print survey_OBSERVATION table diffs and write to a log file in LOG_DIR (if exists)\n",
        "ac  - append data from source to target db WITH corrections file.",
        "dr  - do a dry run; append data from source to target db WITH corrections file;",
        "      write diff output to file in LOG directory. If log file does not exist,",
        "      print to screen.\n",
        "aw  - append data from source to target db WITHOUT corrections file.\n",
        "q   - quit this program\n",
        sep = "\n")

    cat("Type an option and press the 'Enter' key.\n")

    opt <- readline("Enter an option: ")

    if (opt == 'lp') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_lu_diffs(src_conn, tgt_conn)
      print_diffs(diffs)
      if (has_log_dir) {
          cat("Writing DIFF log file...\n")
          write_diffs(diffs, LOG_DIR)
      }
      cat("\nDone!\n\n")
    } else if (opt == 'lw') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_lu_diffs(src_conn, tgt_conn)
      if (has_log_dir) {
          cat("Writing DIFF log file...\n")
          write_diffs(diffs, LOG_DIR)
      } else {
        cat("\nLog directory path not provided. Log file not created.\n\n")
      }
      cat("\nDone!\n\n")
    } else if (opt == 'ew') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_event_diffs(src_conn, tgt_conn)
      print_diffs(diffs)
      if (has_log_dir) {
          cat("Writing DIFF log file...\n")
          write_diffs(diffs, LOG_DIR)
      }
      cat("\nDone!\n\n")
    } else if (opt == 'ewa') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_event_diffs(src_conn, tgt_conn, TRUE)
      print_diffs(diffs)
      if (has_log_dir) {
          cat("Writing DIFF log file...\n")
          write_diffs(diffs, LOG_DIR)
      }
      cat("\nDone!\n\n")
    } else if (opt == 'ow') {
      cat("Finding lookup table diffs...\n")
      diffs <- identify_obs_diffs(src_conn, tgt_conn)
      print_diffs(diffs)
      if (has_log_dir) {
          cat("Writing DIFF log file...\n")
          write_diffs(diffs, LOG_DIR)
      }
      cat("\nDone!\n\n")
    } else if (opt == 'ac') {
      if (has_corr_file) {
        cat("Appending data...\n")
        append_peregrine_data(src_conn, tgt_conn, logger, CORRECTIONS_CSV)
        if (has_log_dir) {
            cat("Writing APPEND log file...\n")
            write_log(logger$log, LOG_DIR)
        }
      } else {
        cat("\nCorrections file path not provided. Data not appended.\n\n")
      }
      cat("\nDone!\n\n")
    } else if (opt == 'dr') {
      if (has_corr_file) {
        cat("Appending data...\n")
        diffs <- append_peregrine_data(src_conn, tgt_conn, logger, CORRECTIONS_CSV, dry_run = TRUE)
        if (has_log_dir) {
            cat("Writing DRY-RUN DIFF log file...\n")
            write_diffs(diffs, LOG_DIR, dry_run = TRUE)
            cat("Writing APPEND log file...\n")
            write_log(logger$log, LOG_DIR)
        } else {
            print_diffs(diffs)
        }
      } else {
        cat("\nCorrections file path not provided. Data not appended.\n\n")
      }
      cat("\nDone!\n\n")
    } else if (opt == 'aw') {
      cat("Appending data...\n")
      append_peregrine_data(src_conn, tgt_conn, logger)
      if (has_log_dir) {
        cat("Writing APPEND log file...\n")
        write_log(logger$log, LOG_DIR)
      }
      cat("\nDone!\n\n")
    } else if (opt == 'q') {
      cat("Exiting...\n")
      break
    } else {
      cat("*** Must choose an option ***\n")
    }
  }
}

has_path <- function(path) {
  if (is.null(path)) {
    return(FALSE)
  } else if (file.exists(path)) {
    return(TRUE)
  }
}
