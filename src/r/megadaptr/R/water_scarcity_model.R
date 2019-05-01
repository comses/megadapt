#function to calculate the scarcity index as implemented by Estrada nd Grave
# from LANCIS

#' calculate scarcity index from a set of landscape attributes from the study area value functions
#'
#' @param study_data data.frame with variables population_size houses without supply
#' @param value_function_config List of functions determining the impact of population
#' density, potable water accessibility and potable water critical zone status on the
#' water scarcity index
#' @return water scarcity index by census block
update_water_scarcity_index <- function(study_data, value_function_config) {
  sewer_age <- value_function_config$sewer_age
  shortage_age <- value_function_config$shortage_age
  shortage_failures <- value_function_config$shortage_failures
  hydraulic_pressure_failure <-
    value_function_config$hydraulic_pressure_failure
  subsidence <- value_function_config$subsidence


  fv_pob_ageb <-
    sapply((study_data$poblacion / study_data$area) * 1000000,
           FUN = logistic_vf,
           k = 0.3,
           xmin = min(study_data$poblacion),
           xmax = max(study_data$poblacion),
           center = 15000
    )


  fv_viviendas_sagua <- sapply(
    study_data$falta_dist,
    FUN = gaussian,
    a = 30,
    xmin = min(study_data$falta_dist),
    xmax = max(study_data$falta_dist),
    center = 0
  )

  fv_zonas_crit = sapply(
    study_data$critic_z,
    FUN = gaussian,
    a = 23,
    center = 0,
    xmin = min(study_data$critic_z),
    xmax = max(study_data$critic_z)
  )


  0#  zonas criticas no estan en el dataframe
  fv_num_cisternas = 0  #  study_data$tanks   add cisternas

  fv_num_cisternas <- sapply(
    study_data$tanks,
    FUN = convexa_creciente,
    gama = .05,
    xmax = max(study_data$tanks, na.rm = T),
    xmin = min(study_data$tanks, na.rm = T)
  )

  fv_dias_sagua = sapply(
    study_data$wo_water,
    FUN = gaussian,
    a = 15,
    center = 0,
    xmin = min(study_data$wo_water, na.rm = T),
    xmax = max(study_data$wo_water, na.rm = T)
  )


  fv_ingreso <- sapply(
    study_data$income_pc,
    FUN = convexa_creciente,
    gama = .015,
    xmin = min(study_data$income_pc, na.rm = T),
    xmax = max(study_data$income_pc, na.rm = T)
  )

  l = 6
  scarcity_index = (1/l)*(fv_pob_ageb + fv_zonas_crit + fv_num_cisternas + fv_ingreso +  fv_dias_sagua  +  fv_viviendas_sagua)

  tibble::tibble(
    ageb_id = study_data$ageb_id,
    scarcity_index = scarcity_index)
}

water_scarcity_index_component <- list(
  initialize = function(study_data, value_function_config) {
    study_data %>%
      dplyr::inner_join(update_water_scarcity_index(study_data = study_data, value_function_config = value_function_config), by = PK_JOIN)
  },
  transition = update_water_scarcity_index
)
