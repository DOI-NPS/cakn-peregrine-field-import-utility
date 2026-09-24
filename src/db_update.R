
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

print_diffs <- function(diffs) {
  cat(get_session_info())

  for (tbl_name in names(diffs)) {
    cat(get_table_header(tbl_name))
    print(diffs[[tbl_name]])
  }
}

write_diffs <- function(diffs, dir_path) {
  file <- file.path(dir_path, paste0("diff_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

  success <- file.create(file, showWarnings = FALSE)

  if (success) {
    sink(file, append = TRUE)
    on.exit(sink())  # ensures sink closes even if an error occurs
    print_diffs(diffs)
  }

  return(success)
}

## Corrects src_survey_event or src_survey_observation based on a CSV of
## updated lookup values as supplied by the user. This CSV allows the user
## to provide mappings from values in tgt to values in src for any lookup
## tables that are out of synch between the two databases. Rather than updating
## the lookup table itself, the approach is to update the values in
## src_survey_event and src_survey_observation in R memory prior to inserting
## records into tgt.
## Parameter(s):
##   src_table: table in R memory to modify.
##   src_table_name: either "survey_event" or "survey_observation".
##   src_conn: connection object to src database.
##   corrections_csv: file path to CSV explaining which id values to update from
## each lookup table. Must contain three columns: "lu_table_name", "field_id_value",
## and "new_id_value". "lu_table_name" indicates which lookup table is out of synch
## between src and tgt, "field_id_value" is the value from the field database, and
## "new_id_value" is the value we want to update it to. If this is not a string, no
## updates are made.
update_src <- function(src_table, src_table_name, src_conn, corrections_csv = FALSE) {

  if (is.character(corrections_csv)) {

    corrections <- readr::read_csv(corrections_csv) |>
      arrange(lu_table_name, desc(new_id_value)) # Sort to avoid overwriting changed values

    # Handle CSV formatting errors
    if (!identical(colnames(corrections), c("lu_table_name", "field_id_value", "new_id_value"))) {
      stop("Corrections csv must contain three columns with the following names: lu_table_name, field_id_value, new_id_value")
    }

    # Handle invalid src_table_name
    if (src_table_name != "survey_event" & src_table_name != "survey_observation") {
      stop("src_table_name must be 'survey_event' or 'survey_observation'")
    }

    # Get list of foreign keys in src_table
    fk_list <- DBI::dbGetQuery(src_conn, paste0("PRAGMA foreign_key_list(", src_table_name, ");"))

    # Loop over rows from csv
    for (i in seq_len(nrow(corrections))) {
      corr_tbl_name <- corrections$lu_table_name[i]
      corr_old_id <- corrections$field_id_value[i]
      corr_new_id <- corrections$new_id_value[i]

      # Scenario: table is in foreign key list
      if (corr_tbl_name %in% fk_list$table) {
        fk_flt <- fk_list |>
          filter(table == corr_tbl_name)

        # Loop over relevant relationships for given lookup table
        for (j in seq_len(nrow(fk_flt))) {
          fk_from <- fk_flt$from[j]
          src_table <- src_table |>
            mutate(!!fk_from := if_else(.data[[fk_from]] == corr_old_id,
                                        corr_new_id,
                                        .data[[fk_from]]))
        }
      } # Scenario: table is site (MVF and also not in foreign key list)
      else if (corr_tbl_name == "site") {
        if (src_table_name == "survey_event") {
          src_table <- src_table |>
            mutate(site_id = if_else(site_id == corr_old_id,
                                     corr_new_id,
                                     site_id),
                   concurrent_obs_sites = str_replace_all(concurrent_obs_sites,
                                                          paste0("(\\{|,)", corr_old_id, "(\\}|,)"),
                                                          paste0("\\1", corr_new_id, "\\2")))
        } else if (src_table_name == "survey_observation") {
          src_table <- src_table |>
            mutate(additional_sites_used = str_replace_all(additional_sites_used,
                                                           paste0("(\\{|,)", corr_old_id, "(\\}|,)"),
                                                           paste0("\\1", corr_new_id, "\\2")))
        }
      } # Scenario: table is activity_lu (MVF)
      else if (corr_tbl_name == "activity_lu" & src_table_name == "survey_observation") {
        src_table <- src_table |>
          mutate(activity_code = str_replace_all(activity_code,
                                                 paste0("(\\{|,)", corr_old_id, "(\\}|,)"),
                                                 paste0("\\1", corr_new_id, "\\2")))
      } # Other tables should raise warnings as our code was not designed (yet) to handle them
      else {
        warning(src_table_name, " not configured to handle updates to ", corr_tbl_name,
             ". Review code if updates are expected here.")
      }
    }
    return(src_table)
  } else {
    return(src_table)
  }
}
