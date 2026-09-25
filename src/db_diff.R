
## Return a list of diffdf objects, for each lookup table comparison.
identify_lu_diffs <- function(src_conn, tgt_conn) {
  # Names of lookup tables to check
  lu_table_names <- c("activity_lu", "nest_content_lu", "nest_observation_lu",
                      "nesting_stage_lu", "observation_estimate_lu", "observer_lu",
                      "site", "species_lu", "truth_lu", "viewing_condition_lu",
                      "visit_lu", "whitewash_lu")

  # Loop over the lookup tables, identifying differences for each table
    lu_diffs <- lapply(lu_table_names, function(table_name) {
      # Grab copies of the lookup tables from tgt and src
      tgt_lu <- get_table(tgt_conn, table_name)
      src_lu <- get_table(src_conn, table_name) |>
        dplyr::select(colnames(tgt_lu)) # select only the columns that are present in tgt to facilitate comparisons

      diffdf::diffdf(src_lu, tgt_lu, suppress_warnings = TRUE)
    })

  names(lu_diffs) <- lu_table_names
  return(lu_diffs)
}

write_diffs <- function(diffs, dir_path, dry_run = FALSE) {
  if (dry_run) {
    file <- file.path(dir_path, paste0("dry_run_diff_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  } else {
    file <- file.path(dir_path, paste0("diff_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  }

  success <- file.create(file, showWarnings = FALSE)

  if (success) {
    sink(file, append = TRUE)
    on.exit(sink())  # ensures sink closes even if an error occurs
    print_diffs(diffs)
  }

  return(success)
}

print_diffs <- function(diffs) {
  cat(get_session_info())

  for (tbl_name in names(diffs)) {
    cat(get_table_header(tbl_name))
    print(diffs[[tbl_name]])
  }
}

get_session_info <- function() {
  info <- paste0(
    "========================================================================================\n",
    "SESSION INFO\n",
    "========================================================================================\n",
    "USER: ", Sys.info()[["user"]], "\n",
    "DATETIME: ", format(Sys.time(), "%B %d, %Y %H:%M:%S"), "\n",
    "NOTE: \n - SRC = .gpkg = BASE \n - TGT = .accdb = COMPARE\n\n")

  return(info)
}

get_table_header <- function(tbl_name) {
  tbl_header <- paste0(
    "========================================================================================\nTABLE:",
    tbl_name,
    "\n========================================================================================\n")

  return(tbl_header)
}
