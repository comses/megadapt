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
    population=0.2,
    tanks=0.2,
    income=0.2  #add water capture
  )
) {
  list(
    weights = weights
  )
}

water_scarcity_index_sensitivity_fnss_create <- function(
  weights = c(
    population=0.2,
    tanks=0.2,
    income=0.2  #add water capture
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
  hydraulic_pressure_failure <- fnss$value_function_config$hydraulic_pressure_failure

  fv_viviendas_sagua <- sapply(
    study_data$household_potable_system_lacking_percent,
    FUN = logistica_invertida,
    center = 0.17,
    k = 0.08349999999999999,
    xmin = min(study_data$household_potable_system_lacking_percent),
    xmax = max(study_data$household_potable_system_lacking_percent)

  )

  fv_zonas_crit = sapply(
    study_data$criticalzone,
    FUN = logistica_invertida,
    k = 1.284,
    center = -0.25,
    xmin = min(study_data$criticalzone),
    xmax = max(study_data$criticalzone)
  )

  #center=1&k=0.255
  fv_dias_sagua = sapply(
    study_data$resident_reports_potable_water_failure_count_per_area,
    FUN = logistica_invertida,
    k = 0.402,
    center = -50.06,
    xmin = min(study_data$resident_reports_potable_water_failure_count_per_area, na.rm = T),
    xmax = max(study_data$resident_reports_potable_water_failure_count_per_area, na.rm = T)
  )

  fv_Age_Infrastructure <- sapply(
    study_data$potable_water_infrastructure_age,
    FUN = logistica_invertida,
    center = 34,
    k = 0.1325,
    xmax = 100,
    xmin = 0
  )

  fv_hid_pressure <- sapply(
    study_data$potable_system_pressure,
    FUN = logistic_vf,
    k = hydraulic_pressure_failure$k,
    center = hydraulic_pressure_failure$center,
    xmax = hydraulic_pressure_failure$max,
    xmin = hydraulic_pressure_failure$min
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
    weights["tanks"]*fv_num_cisternas +
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

