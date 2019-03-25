# data table to save output data
# save variables:
# Age infra
# capacity infra
# Number of days in a year without water supply
# flooding events
# Protests
# Adaptations Ab
# Adaptations F
# Vulneability Index
COLUMNS_TO_SAVE <- c(
  "ageb_id",
  "municipio",
  "antiguedad_dren",
  "antiguedad_dist",
  "encharca",
  "inunda",
  "rejillas",
  "q100",
  "falta_dist",
  "lambdas",
  "days_wn_water_week",
  "days_wn_water_two_weeks",
  "days_wn_water_year",
  "water_scarcity_weekly",
  "scarcity_index_aDD",
  "social_pressure",
  "sensitivity_Ab",
  "sensitivity_D",
  "vulnerability_Ab",
  "vulnerability_D",
  "Interventions_Ab",
  "Interventions_D"
)

# save results
save_TS <- function(study_data,
                    TR,
                    result_prev_time,
                    month,
                    year) {
  study_data %>%
    dplyr::select(!!! COLUMNS_TO_SAVE) %>%
    dplyr::mutate(
      time_sim = TR,
      month_sim = (!! month),
      year_sim = (!! year)
    ) %>%
    dplyr::union_all(result_prev_time)
}

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
