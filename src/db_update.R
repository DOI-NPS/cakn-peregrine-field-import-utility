

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










