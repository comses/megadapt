library(megadaptr)

study_data <- tibble::tibble(
  censusblock_id = 1,
  area = 250000,
  criticalzone = 0.04,
  resident_count = 3000,
  household_potable_system_lacking_percent = 30,
  household_water_storage_tank_percent = 0.0,
  resident_income_per_capita = 4000,
  resident_reports_potable_water_failure_count_per_area = 5,
  potable_water_infrastructure_age = 40,
  potable_system_pressure = 5
)

describe('a water scarcity index', {
  value_function_config <- value_function_config_default()
  water_scarcity_index_sensitivity_fnss <- water_scarcity_index_sensitivity_fnss_create(value_function_config = value_function_config)
  water_scarcity_index_exposure_fnss <- water_scarcity_index_exposure_fnss_create(value_function_config = value_function_config)

  it('should run', {
    water_scarcity_index_sensitivity <- call_fnss(water_scarcity_index_sensitivity_fnss, study_data)
    water_scarcity_index_exposure <- call_fnss(water_scarcity_index_exposure_fnss, study_data)
  })
})

describe('water scarcity config', {
  value_function_config <- value_function_config_default()

  describe('a water scarcity exposure config', {
    config <- water_scarcity_exposure_config_create()
    it('can be used to construct a water scarcity exposure model', {
      water_scarcity_exposure <- water_scarcity_exposure_deserialize(config, value_function_config)
      expect_s3_class(water_scarcity_exposure, 'water_scarcity_index_exposure_fnss')
    })
  })

  describe('a water scarcity sensitivity config', {
    config <- water_scarcity_sensitivity_config_create()
    it('can be used to construct a water scarcity sensitivity model', {
      water_scarcity_sensitivity <- water_scarcity_sensitivity_deserialize(config, value_function_config)
      expect_s3_class(water_scarcity_sensitivity, 'water_scarcity_index_sensitivity_fnss')
    })
  })
})
