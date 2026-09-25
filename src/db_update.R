
## Corrects src_survey_event or src_survey_observation based on a CSV of
## updated lookup values as supplied by the user. This CSV allows the user
## to provide mappings from values in tgt to values in src for any lookup
## tables that are out of synch between the two databases. Rather than updating
## the lookup table itself, the approach is to update the values in
## src_survey_event and src_survey_observation in R memory prior to inserting
## records into tgt.
## Parameter(s):
##   src_tbl_list: list of source tables (tibbles) to modify.
##     - The names of the list must be either "survey_event" or "survey_observation".
##   corrections_csv: file path to CSV explaining which id values to update from
## each lookup table. Must contain three columns: "lu_table_name", "field_id_value",
## and "new_id_value". "lu_table_name" indicates which lookup table is out of synch
## between src and tgt, "field_id_value" is the value from the field database, and
## "new_id_value" is the value we want to update it to. If this is not a string, no
## updates are made.
update_src <- function(src_tbl_list, corrections_csv = FALSE) {

  if (is.character(corrections_csv)) {
    src_tbl_names <- c("survey_event", "survey_observation")

    # Handle invalid src_table_name
    if (! setequal(names(src_tbl_list), src_tbl_names)) {
      stop("src_table_name must be 'survey_event' or 'survey_observation'")
    }

    corrections <- read_corr_file(corrections_csv)

    # Handle CSV formatting errors
    if (! identical(colnames(corrections), c("lu_table_name", "field_id_value", "new_id_value"))) {
      stop("Corrections CSV must contain three columns: lu_table_name, field_id_value, new_id_value")
    }

    fk_df_list <- list("survey_event" = get_event_fk_df(),
                       "survey_observation" = get_obs_fk_df())

    ret_tbl_list <- list()

    # Loop over source tables (tibbles)
    for (tbl_name in src_tbl_names) {
      src_tbl <- src_tbl_list[[tbl_name]]
      fk_df <- fk_df_list[[tbl_name]]

      # Loop over rows from csv
      for (i in seq_len(nrow(corrections))) {
        corr_tbl_name <- corrections$lu_table_name[i]
        corr_old_id <- corrections$field_id_value[i]
        corr_new_id <- corrections$new_id_value[i]

        if (corr_tbl_name %in% fk_df$fk_tbl_name) {
          fk_flt <- fk_df |>
            filter(fk_tbl_name == corr_tbl_name)

          # Loop over relevant relationships for given lookup table
          for (j in seq_len(nrow(fk_flt))) {
            fk_col <- fk_flt$fk_col_name[j]
            is_mvf <- fk_flt$is_mvf[j]

            if (is_mvf) {
              src_tbl <- src_tbl |>
                mutate(!!fk_col :=
                         str_replace_all(.data[[fk_col]],
                                         paste0("(\\{|,)", corr_old_id, "(\\}|,)"),
                                         paste0("\\1", corr_new_id, "\\2")))
            } else {
              src_tbl <- src_tbl |>
                mutate(!!fk_col := if_else(.data[[fk_col]] == corr_old_id,
                                           corr_new_id,
                                           .data[[fk_col]]))
            }
          }
        }
      }
      ret_tbl_list[[tbl_name]] <- src_tbl
    }
    return(ret_tbl_list)
  } else {
    return(src_tbl_list)
  }
}

read_corr_file <- function(corrections_csv) {
    corrections <- readr::read_csv(corrections_csv) |>
      arrange(lu_table_name, desc(new_id_value)) # Sort to avoid overwriting changed values
    return(corrections)
}

get_event_fk_df <- function() {
    fk_df <- tibble(fk_tbl_name = c("whitewash_lu",
                                    "observer_lu",
                                    "observer_lu",
                                    "nest_observation_lu",
                                    "truth_lu",
                                    "site",
                                    "site"),
                    fk_col_name = c("whitewash_lu_id",
                                    "senior_observer",
                                    "scribe_initials",
                                    "nest_observation_lu_id",
                                    "call_broadcast",
                                    "site_id",
                                    "concurrent_obs_sites"),
                    is_mvf = c(FALSE,
                               FALSE,
                               FALSE,
                               FALSE,
                               FALSE,
                               FALSE,
                               TRUE))
    return(fk_df)
}

get_obs_fk_df <- function() {
    fk_df <- tibble(fk_tbl_name = c("species_lu",
                                    "nesting_stage_lu",
                                    "nest_content_lu",
                                    "truth_lu",
                                    "observer_lu",
                                    "activity_lu",
                                    "site"),
                    fk_col_name = c("species_id",
                                    "nesting_stage_lu_id",
                                    "nest_content_id",
                                    "is_nest_count_estimated",
                                    "field_proofed_by",
                                    "activity_code",
                                    "additional_sites_used"),
                    is_mvf = c(FALSE,
                               FALSE,
                               FALSE,
                               FALSE,
                               FALSE,
                               TRUE,
                               TRUE))
    return(fk_df)
}
