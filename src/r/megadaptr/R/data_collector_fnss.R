save_results <- function(study_data,
                    result_prev_time = NULL,
                    year) {
  COLUMNS_TO_SAVE <- c(
    "censusblock_id",
    "runoff_presence",
    "sewer_infrastructure_age",
    "potable_water_infrastructure_age",
    "resident_reports_ponding_count_mean",
    "resident_reports_flooding_count_mean",
    "sewer_system_storm_drain_count",
    "sewer_system_capacity",
    "household_potable_system_lacking_percent",
    "household_sewer_system_lacking_percent",
    "household_days_no_potable_water_per_week_mean",
    "resident_income_per_capita",
    "resident_asset_index",
    "flooding_event_count",
    "flooding_index",
    "ponding_event_count",
    "ponding_index",
    "scarcity_index_exposure",
    "scarcity_index_sensitivity",
    "household_potable_water_sensitivity",
    "household_water_storage_tank_percent",
    "household_sewer_sensitivity",
    "household_resilience",
    "household_potable_water_vulnerability",
    "household_sewer_vulnerability",
    "household_sewer_intervention_count",
    "household_potable_water_invention_count",
    "sacmex_potable_maintenance_intervention_presence",
    "sacmex_sewer_maintenance_intervention_presence",
    "sacmex_potable_new_infrastructure_intervention_presence",
    "sacmex_sewer_new_infrastructure_intervention_presence",
    "non_potable_maintenance",
    "non_potable_new_infrastructure",
    "potable_maintenance",
    "potable_new_infrastructure"
  )

  df <- study_data %>%
    dplyr::select(!!! COLUMNS_TO_SAVE) %>%
    dplyr::mutate(
      year = (!! year)
    )
  if (!is.null(result_prev_time)) {
    return(dplyr::union_all(df, result_prev_time))
  } else {
    return(df)
  }
}
