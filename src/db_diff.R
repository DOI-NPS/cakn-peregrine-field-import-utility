
## Return a list of diffdf objects, for each lookup table comparison.
identify_lu_diffs <- function(src_conn, tgt_conn) {

  lu_table_names <- c("activity_lu", "nest_content_lu", "nest_observation_lu",
                      "nesting_stage_lu", "observation_estimate_lu", "observer_lu",
                      "site", "species_lu", "truth_lu", "viewing_condition_lu",
                      "visit_lu", "whitewash_lu")

  lu_pk_cols <- paste0(lu_table_names, "_id")

  diffs <- identify_diffs(src_conn, tgt_conn, lu_table_names, lu_pk_cols)

  return(diffs)
}

identify_event_diffs <- function(src_conn, tgt_conn, alternate_key = FALSE) {

  if (alternate_key) {
    event_keys <- list(c("site_id", "survey_date", "start_time", "senior_observer"))
    diffs <- identify_diffs(src_conn, tgt_conn, "survey_event", event_keys)
  } else {
    diffs <- identify_diffs(src_conn, tgt_conn, "survey_event", "survey_event_id")
  }

  return(diffs)
}

identify_obs_diffs <- function(src_conn, tgt_conn) {
    diffs <- identify_diffs(src_conn, tgt_conn, "survey_observation", "survey_observation_id")
}

## NOTE on parameters:
##   compare_tbls - a vector of table names
##   compare_keys - a vector of primary key column names,
##                  or a list of vectors.
##                  For example, if there are two or more columns that comprise the primary key,
##                  pass a list of vectors:
##                    keys <- list("a_id", "b_id", c("d_id", "e_id"))
##                  otherwise passing a vector of keys suffice:
##                    keys <- c("a_id", "b_id", "d_id", "e_id")
##                  or, merely, the a key:
##                    keys <- "a_id"
## NOTE: The number of items in compare_tbls and compare_keys must be equal.
identify_diffs <- function(src_conn, tgt_conn, compare_tbls, compare_keys) {

  df_pk_col <- tibble(table_name = compare_tbls, pk_col_name = compare_keys)

  # Loop over the lookup tables, identifying differences for each table
  diffs <- lapply(compare_tbls,
                  function(tbl_name) {
                    # Grab copies of the lookup tables from tgt and src
                    tgt_tbl <- get_table(tgt_conn, tbl_name)
                    src_tbl <- get_table(src_conn, tbl_name)

                    if (tbl_name == "survey_event") {
                      src_tbl <- format_event_tbl(src_tbl)
                    }

                    # select only the columns that are present in tgt to facilitate comparisons
                    src_tbl <- src_tbl |>
                      dplyr::select(colnames(tgt_tbl))
                    pk_cols <- unlist(select(filter(df_pk_col, table_name == tbl_name), pk_col_name))

                    diffdf::diffdf(src_tbl, tgt_tbl, keys = pk_cols, suppress_warnings = TRUE)
                  })

  names(diffs) <- compare_tbls
  return(diffs)
}

format_event_tbl <- function(tbl_df) {
  tbl_df <- tbl_df |>
    select(-geom) |>
    mutate(survey_date = format_date_ak(survey_date)) |>
    mutate(record_inserted_date = format_datetime_utc(record_inserted_date)) |>
    mutate(record_updated_date = format_datetime_utc(record_updated_date))
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
