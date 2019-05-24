resident_determine_infrastructure_suitability <- function(study_data, value_function_config, mental_models, week_of_year) {
  alternative_weights_iz <- mental_models$residents$alternative_weights$iz
  criteria_iz <- as.vector(mental_models$residents$criteria$iz)

  # Water quality
  #vf_WQ <- sapply(study_data$waterquality_index, FUN = water_quality_residents_vf)
  vf_WQ <- study_data$waterquality_index
  # Crecimiento urbano
  vf_UG <- sapply(study_data$urbangrowth *study_data$resident_count, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$urbangrowth, na.rm = T))
  # agua insuficiente
  vf_Agua_insu <- sapply(
    study_data$study_data$scarcity_index,
    FUN = convexa_decreciente,
    xmax = max(study_data$study_data$scarcity_index),
    xmin = 0,
    gama = 0.01975
  )

  # "Desperdicio de agua"
  vf_Desp_A <- sapply(study_data$resident_potable_water_waste_perception, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$resident_potable_water_waste_perception, na.rm = T))
  # fugas
  fv_fugas <- sapply(study_data$falla_dist, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$falla_dist, na.rm = T))

  # falta infrastructura drenaje
  fv_falta <- sapply(100 * study_data$household_sewer_system_lacking_percent,
                     FUN = lack_of_infrastructure_vf,
                     x_max=100*max(study_data$household_sewer_system_lacking_percent))

  # garbage
  vf_garbage <- sapply(study_data$garbage_index,
                       FUN = drainages_clogged_vf,
                       amplitude = 500000,
                       Valor_minimo_Y_en_X = max(study_data$garbage_index))
  # Ponding
  vf_pond <- study_data$ponding_index

  # salud
  vf_H <- sapply(study_data$resident_diarrhea_per_capita, FUN = health_vf,max_x=50,saturation=3)

  # house modification flooding
  C_R_D <- cbind(
    vf_WQ,
    vf_UG,
    rep(1, length(fv_falta)),
    rep(1, length(fv_falta)),
    fv_falta,
    vf_garbage,
    rep(1, length(fv_falta)), # scarcity does not affect floding
    vf_pond,
    vf_H
  )

  # house modification water supply
  C_R_HM <- cbind(
    vf_WQ,
    vf_UG,
    vf_Desp_A,
    fv_fugas,
    fv_falta,
    rep(1, length(fv_falta)),
    vf_Agua_insu,
    rep(1, length(fv_falta)), # flooding do not influence protests or water capture
    vf_H
  )

  distance_ideal_House_mod_lluvia <- sweep(as.matrix(C_R_D[, -c(3, 4, 7)]),
                                           MARGIN = 2,
                                           criteria_iz[-c(3, 4, 7)] / sum(criteria_iz[-c(3, 4, 7)]),
                                           FUN = ideal_distance,
                                           z = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])) # "House modification"
  distance_ideal_House_mod_agua <- sweep(as.matrix(C_R_HM[, -c(6, 8)]),
                                         MARGIN = 2,
                                         criteria_iz[-c(6, 8)] / sum(criteria_iz[-c(6, 8)]),
                                         FUN = ideal_distance,
                                         z = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])) # "House modification"

  list(
    distance_ideal_House_mod_lluvia = distance_ideal_House_mod_lluvia,
    distance_ideal_House_mod_agua = distance_ideal_House_mod_agua
  )
}

resident_fnss_create <- function(value_function_config, mental_model_strategy, half_sensitivity_ab, half_sensitivity_d) {
  config <- list(
    value_function_config = value_function_config,
    mental_model_strategy = mental_model_strategy,
    params = list(
      half_sensitivity_ab = half_sensitivity_ab,
      half_sensitivity_d = half_sensitivity_d
    )
  )
  prepend_class(config, 'resident_fnss')
}

call_fnss.resident_fnss <- function(study_data) {
  mental_models <- mental_model_resident_create()
}
update_residential_infrastructure_investments <- function(study_data, value_function_config, mental_models, params) {
  suitability <- determine_residential_infrastructure_suitability(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models
  )

  # find agebs that will adapt to reduce effects of flooding
  HM_LL <- which(suitability$distance_ideal_House_mod_lluvia > suitability$distance_ideal_House_mod_agua)
  # find agebs that will adapt to reduce effects of water scarcity
  HM_Agua <- which(suitability$distance_ideal_House_mod_lluvia < suitability$distance_ideal_House_mod_agua)

  study_data %>%
    dplyr::mutate(
      house_modifications_D := {
        house_modifications_D[HM_LL] <- house_modifications_D[HM_LL] + 1
        house_modifications_D
      },
      sensitivity_D := {
        sensitivity_D[HM_LL] <- 1 - (house_modifications_D[HM_LL] / (params$half_sensitivity_d + house_modifications_D[HM_LL]))
        sensitivity_D
      },
      house_modifications_Ab := {
        house_modifications_Ab[HM_Agua] <- house_modifications_Ab[HM_Agua] + 1
        house_modifications_Ab
      },
      sensitivity_Ab := {
        sensitivity_Ab[HM_Agua] <- 1 - (house_modifications_Ab[HM_Agua] / (params$half_sensitivity_ab + house_modifications_Ab[HM_Agua]))
        sensitivity_Ab
      },
      tanks := {
        tanks[HM_Agua] <- tanks[HM_Agua] + 1 / params$half_sensitivity_ab
        tanks
      },
      vulnerability_Ab = (sensitivity_Ab * scarcity_index) / (1 + ingreso),
      vulnerability_D = (sensitivity_D * encharca_index) / (1 + ingreso)
    ) %>%
    dplyr::select(
      ageb_id,
      house_modifications_D,
      sensitivity_D,
      house_modifications_Ab,
      sensitivity_Ab,
      tanks,
      vulnerability_Ab,
      vulnerability_D
    )
}

resident_component <- list(
  initialize = function(study_data) {
    study_data %>%
      dplyr::mutate(house_modifications_Ab=0L,
                    house_modifications_D=0L,
                    sensitivity_Ab = 1,
                    sensitivity_D = 1,
                    vulnerability_Ab = 1,
                    vulnerability_D = 1)
  },
  transition = update_residential_infrastructure_investments
)
