#' Create an object of class "water_scarcity_index_fnss".
#'
#' @export
#' @param weights A vector of weigth parameters of length=4.
#' @param value_function_config A set of parameters to configurate value functions
#'
#' @return An object of class 'water_scarcity_index_fnss' to be used as arguments into the ponding index with value functions.


water_scarcity_index_fnss_create <- function(
  weights = c(
    population=0.2,
    houses_without_water=0.2,
    zonas_crit=0.2,
    tanks=0.2,
    days_no_water=0.2,
    income=0.2,
    age_infrastructure=0.2,
    hydra_pressure=0.2
  ),
  value_function_config) {
  #' calculate scarcity index from a set of landscape attributes from the study area value functions
  #'
  #' @export
  #' @param value_function_config List of functions determining the impact of population
  #' density, potable water accessibility and potable water critical zone status on the
  #' water scarcity index
  weights <- weights / sum(weights)
 #   prepend_class(value_function_config, 'water_scarcity_index_fnss')
  prepend_class(weights, 'water_scarcity_index_fnss')
}

call_fnss.water_scarcity_index_fnss <- function(fnss, study_data, ...) {
  #' Water scarcity index calculation
  #'
  #' @export
  #' @method call_fnss water_scarcity_index_fnss
  #' @inheritParams call_fnss
  #' @param study_data data frame with the data of the study area
  #' @return a data frame with field "censusblock_id" and "scarcity_index"
  weights <- fnss

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
  fv_num_cisternas = 0  #  study_data$household_water_storage_tank_percent   add cisternas

  fv_num_cisternas <- sapply(
    study_data$household_water_storage_tank_percent,
    FUN = convexa_creciente,
    gama = .05,
    xmax = max(study_data$household_water_storage_tank_percent, na.rm = T),
    xmin = min(study_data$household_water_storage_tank_percent, na.rm = T)
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

  fv_Age_Infrastructure <- sapply(
    study_data$potable_water_infrastructure_age,
    FUN = logistica_invertida,
    center = 40,
    k = 0.1,
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

  scarcity_index = weights["population"]*fv_pob_ageb +
                   weights["zonas_crit"]*fv_zonas_crit +
                   weights["tanks"]*fv_num_cisternas +
                   weights["income"]*fv_ingreso +
                   weights["days_no_water"]*fv_dias_sagua  +
                   weights["houses_without_water"]*fv_viviendas_sagua +
                   weights["age_infrastructure"]*fv_Age_Infrastructure +
                   weights["hydra_pressure"]*fv_hid_pressure


  tibble::tibble(
    censusblock_id = study_data$censusblock_id,
    scarcity_index = scarcity_index)
}

water_scarcity_initialize <- function(water_scarcity_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(water_scarcity_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}
