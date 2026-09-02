
## Returns a list with an entry for each lookup table used in the field. 
## Each entry in the list is "Identical" if the field (src) copy matches the 
## office (tgt) copy; alternatively, contains table(s) of new rows in the
## field (src) copy and/or office (tgt) copy.
## Parameter(s):
##   src_conn: connection object to src database.
##   tgt_conn: connection object to tgt database.
identify_lu_diffs <- function(src_conn, tgt_conn) {
  # Names of lookup tables to check
  lu_table_names <- c("activity_lu", "nest_content_lu", "nest_observation_lu", 
                      "nesting_stage_lu", "observation_estimate_lu", "observer_lu", 
                      "site", "species_lu", "truth_lu", "viewing_condition_lu", 
                      "visit_lu", "whitewash_lu")
  
  # Loop over the lookup tables, identifying rows that occur only in tgt and only in src
  lu_diffs <- lapply(lu_table_names,
                     function(table_name) {
                       # Grab copies of the lookup tables from tgt and src
                       tgt_lu <- get_table(tgt_conn, table_name)
                       src_lu <- get_table(src_conn, table_name) |>
                         dplyr::select(colnames(tgt_lu)) # select only the columns that are present in tgt to facilitate comparisons
                      
                       # If the lookup tables don't match, report rows that differ
                       if (!identical(src_lu, tgt_lu)) {
                         tgt_new <- dplyr::setdiff(tgt_lu, src_lu)
                         src_new <- dplyr::setdiff(src_lu, tgt_lu)
                        
                         diffs <- list()
                         if (nrow(tgt_new) > 0) {
                           message(table_name, ": Rows exist in target that are absent from source:") 
                           diffs <- append(diffs, list(tgt_new = tgt_new))
                           print(tgt_new)
                           } 
                         if (nrow(src_new) > 0) {
                           message(table_name, ": Rows exist in source that are absent from target:")
                           diffs <- append(diffs, list(src_new = src_new))
                           print(src_new)
                           }
                         diffs
                         } else {
                           "Identical"
                           }
                       })
  names(lu_diffs) <- lu_table_names
  return(lu_diffs)
}



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
    foreign_key_list <- DBI::dbGetQuery(src_conn, paste0("PRAGMA foreign_key_list(", src_table_name, ");"))
    
    # Loop over rows from csv
    for (i in seq_len(nrow(corrections))) {
      
      # Scenario: table is in foreign key list
      if (corrections$lu_table_name[i] %in% foreign_key_list$table) {
        changes <- foreign_key_list |>
          filter(table == corrections$lu_table_name[i])
        
        # Loop over relevant relationships for given lookup table
        for (j in seq_len(nrow(changes))) {
          src_table <- src_table |>
            mutate(!!changes$from[j] := if_else(.data[[changes$from[j]]] == corrections$field_id_value[i], 
                                                corrections$new_id_value[i],
                                                .data[[changes$from[j]]]))
        }
      } # Scenario: table is site (MVF and also not in foreign key list)
      else if (corrections$lu_table_name[i] == "site") { 
        if (src_table_name == "survey_event") {
          src_table <- src_table |>
            mutate(site_id = if_else(site_id == corrections$field_id_value[i],
                                     corrections$new_id_value[i],
                                     site_id),
                   concurrent_obs_sites = str_replace_all(concurrent_obs_sites, 
                                                          paste0("(\\{|,)", corrections$field_id_value[i], "(\\}|,)"),
                                                          paste0("\\1", corrections$new_id_value[i], "\\2")))
        } else if (src_table_name == "survey_observation") {
          src_table <- src_table |>
            mutate(additional_sites_used = str_replace_all(additional_sites_used, 
                                                           paste0("(\\{|,)", corrections$field_id_value[i], "(\\}|,)"),
                                                           paste0("\\1", corrections$new_id_value[i], "\\2")))
        }
      } # Scenario: table is activity_lu (MVF) 
      else if (corrections$lu_table_name[i] == "activity_lu" & src_table_name == "survey_observation") {
        src_table <- src_table |>
          mutate(activity_code = str_replace_all(activity_code, 
                                                 paste0("(\\{|,)", corrections$field_id_value[i], "(\\}|,)"),
                                                 paste0("\\1", corrections$new_id_value[i], "\\2")))
      } # Other tables should raise errors as our code was not designed (yet) to handle them
      else {
        warning(src_table_name, " not configured to handle updates to ", corrections$lu_table_name[i],
             ". Review code if updates are expected here.")
      }
    }
    return(src_table)
  } else{
    return(src_table)
  }
}






