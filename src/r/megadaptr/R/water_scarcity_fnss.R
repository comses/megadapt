water_scarcity_exposure_deserialize <- function(config, value_function_config) {
  config <- do.call(water_scarcity_exposure_config_create, config)
  weights <- as.numeric(config$weights)
  names(weights) <- names(config$weights)
  water_scarcity_index_exposure_fnss_create(
    weights = weights,
    value_function_config = value_function_config)
}

water_scarcity_exposure_config_create <- function(
  weights = list(
    zonas_crit=0.2,
    days_no_water=0.2,
    age_infrastructure=0.2,
    houses_without_water=0.2,
    hydra_pressure=0.2
  )
) {
  list(
    weights = weights
  )
}

#' Create an object of class "water_scarcity_index_fnss".
#'
#' @export
#' @param weights A vector of weigth parameters of length=4.
#' @param value_function_config A set of parameters to configurate value functions
#'
#' @return An object of class 'water_scarcity_index_fnss' to be used as arguments into the ponding index with value functions.
water_scarcity_index_exposure_fnss_create <- function(
  weights = c(
    zonas_crit=0.2,
    days_no_water=0.2,
    age_infrastructure=0.2,
    houses_without_water=0.2,
    hydra_pressure=0.2
  ),
  value_function_config) {
  weights <- weights / sum(weights)
  config <- list(
    weights = weights,
    value_function_config = value_function_config
  )
  prepend_class(config, 'water_scarcity_index_exposure_fnss')
}

water_scarcity_sensitivity_deserialize <-
  function(config, value_function_config) {
    config <- do.call(water_scarcity_sensitivity_config_create, config)
    weights <- as.numeric(config$weights)
    names(weights) <- names(config$weights)
    water_scarcity_index_sensitivity_fnss_create(weights = weights, value_function_config = value_function_config)
  }

water_scarcity_sensitivity_config_create <- function(
  weights = list(
    population=0.5,
    income=0.5
  )
) {
  list(
    weights = weights
  )
}

water_scarcity_index_sensitivity_fnss_create <- function(
  weights = c(
    population=0.5,
    income=0.5
  ),
  value_function_config) {
  weights <- weights / sum(weights)
  config <- list(
    weights = weights,
    value_function_config = value_function_config
  )
  prepend_class(config, 'water_scarcity_index_sensitivity_fnss')
}

call_fnss.water_scarcity_index_exposure_fnss <- function(fnss, study_data, ...) {
  #' Water scarcity index calculation
  #'
  #' @export
  #' @method call_fnss water_scarcity_index_exposure_fnss
  #' @inheritParams call_fnss
  #' @param study_data data frame with the data of the study area
  #' @return a data frame with field "censusblock_id" and "scarcity_index"
  weights <- fnss$weights
  fv_hydraulic_pressure_failure <- fnss$value_function_config$hydraulic_pressure_failure
  fv_infrastructure_age <- fnss$value_function_config$infrastructure_age
  fv_fix_failure_days <- fnss$value_function_config$fix_failure_days
  fv_critical_zones <- fnss$value_function_config$critical_zones
  fv_potable_lacking <- fnss$value_function_config$potable_lacking


  f_lacking_potable <- function_from(fv_potable_lacking)
  fv_viviendas_sagua <- f_lacking_potable(
      study_data$household_potable_system_lacking_percent,
      fv_potable_lacking)

  f_critical_zones <- function_from(fv_critical_zones)
  fv_zonas_crit = f_critical_zones(
    study_data$criticalzone,
    fv_critical_zones
  )

  f_dias_sagua <- function_from(fv_fix_failure_days)
  fv_dias_sagua = f_dias_sagua(
    study_data$resident_reports_potable_water_failure_count_per_area,
    fv_fix_failure_days
  )

  f_infrastructure_age <- function_from(fv_infrastructure_age)
  fv_Age_Infrastructure <- f_infrastructure_age(
    study_data$potable_water_infrastructure_age,
    fv_infrastructure_age
  )

  f_hid_pressure <- function_from(fv_hydraulic_pressure_failure)
  fv_hid_pressure <- f_hid_pressure(
    study_data$potable_system_pressure,
    fv_hydraulic_pressure_failure
  )



  scarcity_index_exposure = weights["zonas_crit"]*fv_zonas_crit +
                    weights["days_no_water"]*fv_dias_sagua  +
                   weights["age_infrastructure"]*fv_Age_Infrastructure +
                weights["houses_without_water"]*fv_viviendas_sagua +
                   weights["hydra_pressure"]*fv_hid_pressure

  tibble::tibble(
    censusblock_id = study_data$censusblock_id,
    scarcity_index_exposure=scarcity_index_exposure
    )
}

call_fnss.water_scarcity_index_sensitivity_fnss <- function(fnss, study_data, ...) {
  #' Water scarcity index calculation
  #'
  #' @export
  #' @method call_fnss water_scarcity_index_sensitivity_fnss
  #' @inheritParams call_fnss
  #' @param study_data data frame with the data of the study area
  #' @return a data frame with field "censusblock_id" and "scarcity_index"
  weights <- fnss$weights

  #population
  fv_pob_ageb <-
    sapply((study_data$resident_count / study_data$area) * 1000000,
           FUN = logistic_vf,
           k = 0.3,
           xmin = min(study_data$resident_count),
           xmax = max(study_data$resident_count),
           center = 15000
    )



  fv_num_cisternas <- sapply(
    study_data$household_water_storage_tank_percent,
    FUN = convexa_creciente,
    gama = .05,
    xmax = max(study_data$household_water_storage_tank_percent, na.rm = T),
    xmin = min(study_data$household_water_storage_tank_percent, na.rm = T)
  )



  fv_ingreso <- sapply(
    study_data$resident_income_per_capita,
    FUN = convexa_creciente,
    gama = .015,
    xmin = min(study_data$resident_income_per_capita, na.rm = T),
    xmax = max(study_data$resident_income_per_capita, na.rm = T)
  )


  scarcity_index_sensitivity = weights["population"]*fv_pob_ageb +
    weights["income"]*fv_ingreso

   #need to include water capture


  tibble::tibble(
    censusblock_id = study_data$censusblock_id,
    scarcity_index_sensitivity = scarcity_index_sensitivity
  )
}


water_scarcity_index_exposure_initialize <- function(water_scarcity_index_exposure_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(water_scarcity_index_exposure_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}

water_scarcity_index_sensitivity_initialize <- function(water_scarcity_index_sensitivity_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(water_scarcity_index_sensitivity_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}

