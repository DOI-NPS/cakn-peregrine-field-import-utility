
append_peregrine_data <- function(src_conn, tgt_conn, logger, corrections_csv = NULL, dry_run = FALSE) {
  # ---------------------------
  # Read data
  # ---------------------------

  logger$log <- paste0(get_log_info(), logger$log)
  logger$log <- paste0(logger$log, "Append data logging...", "\n")
  logger$log <- paste0(logger$log, "- Get SOURCE table survey_event data.\n")

  src_survey_event <- get_table(src_conn, "survey_event") |>
    select(-geom)

  logger$log <- paste0(logger$log, "- Get SOURCE table survey_observation data.\n")

  src_survey_obs <- get_table(src_conn, "survey_observation")


  src_tbl_list <- list("survey_event" = src_survey_event,
                       "survey_observation" = src_survey_obs)

  if (is.null(corrections_csv)) {
    logger$log <- paste0(logger$log, "- CORRECTIONS_CSV file not supplied. Source table survey_event NOT updated.\n")
  } else {
    logger$log <- paste0(logger$log, "- UPDATE survey_event data with CORRECTIONS_CSV file.\n")
  }

  src_tbl_list <- update_src(src_tbl_list, corrections_csv)


  if (dry_run) {
    logger$log <- paste0(logger$log, "- Initiating DRY-RUN\n")

    event_diff <- diffdf::diffdf(src_survey_event, src_tbl_list$survey_event,
                                 suppress_warnings = TRUE,
                                 strict_numeric = FALSE)
    obs_diff <- diffdf::diffdf(src_survey_obs, src_tbl_list$survey_observation,
                               suppress_warnings = TRUE,
                               strict_numeric = FALSE)

    diffs <- list("survey_event" = event_diff, "survey_observation" = obs_diff)

    logger$log <- paste0(logger$log, "- Finished DRY-RUN\n")

    return(diffs)
  }

  src_survey_event <- src_tbl_list$survey_event
  src_survey_obs <- src_tbl_list$survey_observation

  max_tgt_event_id <- get_query(tgt_conn,
                                paste("SELECT max(survey_event_id) AS max_id",
                                      "FROM survey_event;"))$max_id
  max_tgt_obs_id <- get_query(tgt_conn,
                              paste("SELECT max(survey_observation_id) AS max_id",
                                    "FROM survey_observation;"))$max_id

  tgt_event_id_map <- create_map(src_survey_event$survey_event_id,
                                 max_tgt_event_id + 1)
  tgt_obs_id_map <-create_map(src_survey_obs$survey_observation_id,
                              max_tgt_obs_id + 1)

  ## Map new ID's. Format datetime columns.
  src_survey_event <- src_survey_event |>
    dplyr::mutate(survey_event_id = get_map_value(tgt_event_id_map, survey_event_id)) |>
    dplyr::mutate(record_inserted_date = format_datetime_utc(record_inserted_date)) |>
    dplyr::mutate(record_updated_date = format_datetime_utc(record_updated_date))

  src_survey_obs <- src_survey_obs |>
    dplyr::mutate(survey_event_id = get_map_value(tgt_event_id_map, survey_event_id)) |>
    dplyr::mutate(survey_observation_id = get_map_value(tgt_obs_id_map, survey_observation_id)) |>
    dplyr::mutate(record_inserted_date = format_datetime_utc(record_inserted_date)) |>
    dplyr::mutate(record_updated_date = format_datetime_utc(record_updated_date))

  # Drop extraneous columns
  append_survey_event <- src_survey_event |>
    dplyr::select(-c(survey_event_uuid, show_concurrent_obs_sites, # not in database
                     survey_year, survey_month, # calculated
                     concurrent_obs_sites, # MVF
                     record_proofed_date))

  append_survey_obs <- src_survey_obs |>
    dplyr::select(-c(survey_observation_uuid, show_activity, survey_event_uuid, show_additional_sites_used, # not in database
                     activity_code, additional_sites_used, # MVFs
                     record_proofed_date))

  # ---------------------------
  # Append to database
  # ---------------------------
  # Begin transaction
  DBI::dbBegin(tgt_conn)

  tryCatch(
  {
    # Append tables
    # batch_rows = 1 required for this to work in access: https://stackoverflow.com/questions/77002622/write-a-data-frame-to-an-existing-access-accdb-table-in-r

    logger$log <- paste0(logger$log, "- APPEND source event data to target table survey_event.\n")
    DBI::dbAppendTable(tgt_conn, "survey_event", append_survey_event, batch_rows = 1)

    logger$log <- paste0(logger$log, "- APPEND source observation data to target table survey_observation.\n")
    DBI::dbAppendTable(tgt_conn, "survey_observation", append_survey_obs, batch_rows = 1)

    logger$log <- paste0(logger$log, "- UPDATE target event and observation table MVF columns.\n")

    # Insert data into multi-valued fields (MVF)
    insert_mvf(con = tgt_conn,
               tgt_table_name = "survey_event",
               r_table = src_survey_event,
               mvf_column_name = "concurrent_obs_sites",
               id_column_name = "survey_event_id")
    insert_mvf(con = tgt_conn,
               tgt_table_name = "survey_observation",
               r_table = src_survey_obs,
               mvf_column_name = "activity_code",
               id_column_name = "survey_observation_id")
    insert_mvf(con = tgt_conn,
               tgt_table_name = "survey_observation",
               r_table = src_survey_obs,
               mvf_column_name = "additional_sites_used",
               id_column_name = "survey_observation_id")

    DBI::dbCommit(tgt_conn)

    msg <- "Success -- changes committed."
    message(msg)
    logger$log <- paste0(logger$log, "\n", msg, "\n")
  },
  error = function(e) {
    dbRollback(tgt_conn)

    msg <- paste0("Error occurred, changes rolled back: ", e$message)
    message(msg)
    logger$log <- paste0(logger$log, "\n", msg, "\n")
  }
  )
}

## Purpose: return a named vector to act as a dictionary.
## - The given 'keys' are mapped to a new list of sequential numbers starting with
##   'start_num'.
create_map <- function(keys, start_num) {
  map <- setNames(seq(start_num,
                      length.out = length(keys)),
                  keys)
  return(map)
}

get_map_value <- function(map, key) {
  value <- unname(map[as.character(key)])
  return(value)
}

insert_mvf <- function(con, tgt_table_name, r_table, mvf_column_name, id_column_name) {
  # Loop over the records
  for (i in seq_len(nrow(r_table))) {
    # Account for NA values
    if (!is.na(r_table[[mvf_column_name]][i])) {
      # Identify id and values required for update
      id <- r_table[[id_column_name]][i]
      mvf_values <- remove_mvf_braces(r_table[[mvf_column_name]][i])
      # Loop over the entries in the MVF
      for (j in seq_along(mvf_values)) {
        # Individual updates in database for each value in mvf_values
        sql <- paste0("INSERT INTO ", tgt_table_name, " (",  mvf_column_name, ".Value)
                       VALUES ('", mvf_values[j], "')
                       WHERE ", id_column_name, " = ", id, ";")
        DBI::dbExecute(con, sql)
      }
    }
  }
}

remove_mvf_braces <- function(mvf_str) {
  mvf_values <- mvf_str |>
    str_sub(2, -2) |>
    str_split_1(",")

  return(mvf_values)
}
