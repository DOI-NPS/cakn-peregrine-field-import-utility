

insert_mvf <- function(con, db_table_name, r_table, mvf_column_name, id_column_name) {
  # Loop over the records
  for (i in seq_len(nrow(r_table))) {
    # Account for NA values
    if (!is.na(r_table[[mvf_column_name]][i])) {
      # Identify id and values required for update
      id <- r_table[[id_column_name]][i]
      mvf_values <- r_table[[mvf_column_name]][i] |>
        str_sub(2, -2) |>
        str_split_1(",")
      # Loop over the entries in the MVF
      for (j in seq_along(mvf_values)) {
        # Individual updates in database for each value in mvf_values
        sql <- paste0("INSERT INTO ", db_table_name, " (",  mvf_column_name, ".Value)
                       VALUES ('", mvf_values[j], "')
                       WHERE ", id_column_name, " = ", id, ";")
        DBI::dbExecute(con, sql)
      }
    }
  }
}

append_peregrine_data <- function(db_conn, gpkg_conn) {
  # ---------------------------
  # Read data
  # ---------------------------

  gpkg_survey_event <- get_table(gpkg_conn, "survey_event")
  gpkg_survey_observation <- get_table(gpkg_conn, "survey_observation")

  gpkg_survey_event <- select(gpkg_survey_event, -geom)

  db_survey_event <- get_table(db_conn, "survey_event")
  db_survey_observation <- get_table(db_conn, "survey_observation")

  # ---------------------------
  # Prepare data
  # ---------------------------
  # Current maximum id values for survey_event and survey_observation in db

  max_db_survey_event_id <- max(db_survey_event$survey_event_id)
  max_db_survey_observation_id <- max(db_survey_observation$survey_observation_id)

  ## Purpose: return a named vector to act as a dictionary.
  ## - The given 'keys' are mapped to a new list of sequential numbers starting with
  ##   'start_num'.
  create_map <- function(keys, start_num) {
    map <- setNames(seq(start_num,
                        length.out = length(keys)),
                    keys)
    return(map)
  }

  db_event_id_map <- create_map(gpkg_survey_event$survey_event_id,
                                max_db_survey_event_id + 1)
  db_obs_id_map <-create_map(gpkg_survey_observation$survey_observation_id,
                             max_db_survey_observation_id + 1)

  get_map_value <- function(map, key) {
    value <- unname(map[as.character(key)])
    return(value)
  }

  format_datetime <- function(dt) {
    as.POSIXct(dt,
               "%Y-%m-%dT%H:%M:%S",
               tz="America/Anchorage")
  }

  ## Map new ID's. Format datetime columns.
  gpkg_survey_event <- gpkg_survey_event |>
    dplyr::mutate(survey_event_id = get_map_value(db_event_id_map, survey_event_id)) |>
    dplyr::mutate(record_inserted_date = format_datetime(record_inserted_date)) |>
    dplyr::mutate(record_updated_date = format_datetime(record_updated_date))

  gpkg_survey_observation <- gpkg_survey_observation |>
    dplyr::mutate(survey_event_id = get_map_value(db_event_id_map, survey_event_id)) |>
    dplyr::mutate(survey_observation_id = get_map_value(db_obs_id_map, survey_observation_id)) |>
    dplyr::mutate(record_inserted_date = format_datetime(record_inserted_date)) |>
    dplyr::mutate(record_updated_date = format_datetime(record_updated_date))


  # Drop extraneous columns
  append_survey_event <- gpkg_survey_event |>
    dplyr::select(-c(survey_event_uuid, show_concurrent_obs_sites, # not in database
                     survey_year, survey_month, # calculated
                     concurrent_obs_sites, # MVF
                     record_proofed_date))

  append_survey_observation <- gpkg_survey_observation |>
    dplyr::select(-c(survey_observation_uuid, show_activity, survey_event_uuid, show_additional_sites_used, # not in database
                     activity_code, additional_sites_used, # MVFs
                     record_proofed_date))

  # ---------------------------
  # Append to database
  # ---------------------------
  # Begin transaction
  DBI::dbBegin(db_conn)

  tryCatch(
  {
    # Append tables
    # batch_rows = 1 required for this to work in access: https://stackoverflow.com/questions/77002622/write-a-data-frame-to-an-existing-access-accdb-table-in-r

    DBI::dbAppendTable(db_conn, "survey_event", append_survey_event, batch_rows = 1)
    DBI::dbAppendTable(db_conn, "survey_observation", append_survey_observation, batch_rows = 1)

    # Insert data into multi-valued fields (MVF)
    insert_mvf(con = db_conn,
               db_table_name = "survey_event",
               r_table = gpkg_survey_event,
               mvf_column_name = "concurrent_obs_sites",
               id_column_name = "survey_event_id")
    insert_mvf(con = db_conn,
               db_table_name = "survey_observation",
               r_table = gpkg_survey_observation,
               mvf_column_name = "activity_code",
               id_column_name = "survey_observation_id")
    insert_mvf(con = db_conn,
               db_table_name = "survey_observation",
               r_table = gpkg_survey_observation,
               mvf_column_name = "additional_sites_used",
               id_column_name = "survey_observation_id")

    DBI::dbCommit(db_conn)
    message("Success — changes committed.")
  },
  error = function(e) {
    dbRollback(db_conn)
    message("Error occurred, changes rolled back: ", e$message)
  }
  )

}
