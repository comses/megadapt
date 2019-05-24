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
  "censusblock_id",
  "geographic_id",
  "sewer_infrastructure_age",
  "potable_water_infrastructure_age",
  "resident_reports_ponding_per_year",
  "resident_reports_flooding_per_year",
  "sewer_system_storm_drain_count",
  "non_potable_capacity",
  "household_potable_system_lacking_percent",
  "household_sewer_system_lacking_percent",
  "household_days_no_potable_water_per_week_mean",
  "resident_income_per_capita",
  "encharca_index",
  "scarcity_index",
  "household_potable_water_sensitivity",
  "household_water_storage_tank_available_percent",
  "household_sewer_sensitivity",
  "household_potable_water_vulnerability",
  "household_sewer_vulnerability",
  "sacmex_potable_intervention_count",
  "sacmex_sewer_intervention_count"
)

# save results
save_TS <- function(study_data,
                    result_prev_time = NULL,
                    year) {
  df <- study_data %>%
    dplyr::select(!!! COLUMNS_TO_SAVE) %>%
    dplyr::mutate(
      time_sim = TR,
      month_sim = (!! month),
      year_sim = (!! year)
    )
  if (!is.null(result_prev_time)) {
    return(dplyr::union_all(df, result_prev_time))
  } else {
    return(df)
  }
}
