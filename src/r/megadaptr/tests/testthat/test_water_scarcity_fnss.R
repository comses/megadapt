library(megadaptr)

study_data <- tibble::tibble(
  censusblock_id = 1,
  area = 250000,
  criticalzone = 0.04,
  resident_count = 3000,
  household_potable_system_lacking_percent = 30,
  household_water_storage_tank_percent = 0.0,
  resident_income_per_capita = 4000,
  resident_reports_potable_water_failure_count_per_area = 5
)

describe('a water scarcity index', {
  value_function_config <- value_function_config_default()
  water_scarcity_index_fnss <- water_scarcity_index_fnss_create(value_function_config)

  it('should run', {
    water_scarcity_index <- call_fnss(water_scarcity_index_fnss, study_data)
  })
})
