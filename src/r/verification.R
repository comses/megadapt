compare_results <- function(new_results, old_results, base_cols) {
  colnames_new_results <- setdiff(colnames(new_results), base_cols)
  colnames_old_results <- setdiff(colnames(old_results), base_cols)
  common_colnames <- intersect(colnames_new_results, colnames_old_results)
  
  comparison <- tibble::as_tibble(new_results[base_cols])
  for (colname in common_colnames) {
    col_same <- new_results[[colname]] == old_results[[colname]] | (is.na(new_results[[colname]]) & is.na(old_results[[colname]]))
    if (!(all(col_same))) {
      comparison[[paste0("new_", colname)]] <- new_results[[colname]]
      comparison[[paste0("old_", colname)]] <- old_results[[colname]]
      comparison[[paste0("same_", colname)]] <- col_same
    }
  }
  
  list(comparison = comparison, 
       missing_new_columns = setdiff(colnames_new_results, colnames_old_results),
       missing_old_columns = setdiff(colnames_old_results, colnames_new_results))
}