#function to calculate the scarcity index as implemented by Estrada nd Grave
# from LANCIS

water_scarcity_index_fnss_create <- function(value_function_config) {
  #' calculate scarcity index from a set of landscape attributes from the study area value functions
  #'
  #' @export
  #' @param value_function_config List of functions determining the impact of population
  #' density, potable water accessibility and potable water critical zone status on the
  #' water scarcity index
  prepend_class(value_function_config, 'water_scarcity_index_fnss')
}

call_fnss.water_scarcity_index_fnss <- function(water_scarcity_index_fnss, study_data) {
  #' Water scarcity index calculation
  #'
  #' @export
  #' @method call_fnss water_scarcity_index_fnss
  value_function_config <- water_scarcity_index_fnss
  sewer_age <- value_function_config$sewer_age
  shortage_age <- value_function_config$shortage_age
  shortage_failures <- value_function_config$shortage_failures
  hydraulic_pressure_failure <-
    value_function_config$hydraulic_pressure_failure
  subsidence <- value_function_config$subsidence

  #population
  fv_pob_ageb <-
    sapply((study_data$resident_count / study_data$area) * 1000000,
           FUN = logistic_vf,
           k = 0.3,
           xmin = min(study_data$resident_count),
           xmax = max(study_data$resident_count),
           center = 15000
    )


  fv_viviendas_sagua <- sapply(
    study_data$household_potable_system_lacking_percent,
    FUN = gaussian,
    a = 30,
    xmin = min(study_data$household_potable_system_lacking_percent),
    xmax = max(study_data$household_potable_system_lacking_percent),
    center = 0
  )

  fv_zonas_crit = sapply(
    study_data$criticalzone,
    FUN = gaussian,
    a = 23,
    center = 0,
    xmin = min(study_data$criticalzone),
    xmax = max(study_data$criticalzone)
  )


  0#  zonas criticas no estan en el dataframe
  fv_num_cisternas = 0  #  study_data$tanks   add cisternas

  fv_num_cisternas <- sapply(
    study_data$household_water_storage_tank_available_percent,
    FUN = convexa_creciente,
    gama = .05,
    xmax = max(study_data$household_water_storage_tank_available_percent, na.rm = T),
    xmin = min(study_data$household_water_storage_tank_available_percent, na.rm = T)
  )

  fv_dias_sagua = sapply(
    study_data$resident_reports_potable_water_failure_count_per_area,
    FUN = gaussian,
    a = 15,
    center = 0,
    xmin = min(study_data$resident_reports_potable_water_failure_count_per_area, na.rm = T),
    xmax = max(study_data$resident_reports_potable_water_failure_count_per_area, na.rm = T)
  )


  fv_ingreso <- sapply(
    study_data$resident_income_per_capita,
    FUN = convexa_creciente,
    gama = .015,
    xmin = min(study_data$resident_income_per_capita, na.rm = T),
    xmax = max(study_data$resident_income_per_capita, na.rm = T)
  )

  scarcity_index = 1/6*(fv_pob_ageb + fv_zonas_crit + fv_num_cisternas + fv_ingreso +  fv_dias_sagua  +  fv_viviendas_sagua)

  tibble::tibble(
    ageb_id = study_data$censusblock_id,
    scarcity_index = scarcity_index)
}
