resident_determine_infrastructure_suitability <-
  function(study_data,
           value_function_config,
           mental_models) {
    alternative_weights_iz <-
      mental_models$alternative_weights$iz
    criteria_iz <- as.vector(mental_models$criteria$iz)

    # Water quality
    vf_WQ <- study_data$waterquality_index
    # Crecimiento urbano
    vf_UG <-
      sapply(
        study_data$urbangrowth * study_data$resident_count,
        FUN = Value_Function_cut_offs,
        xcuts = c(0.5, 0.75, 0.875, 0.937),
        ycuts = c(1, 0.8, 0.6, 0.4, 0.2),
        xmax = max(study_data$urbangrowth, na.rm = T)
      )
    # agua insuficiente
    vf_Agua_insu <- sapply(
      study_data$scarcity_index_sensitivity,
      FUN = convexa_decreciente,
      xmax = max(study_data$scarcity_index_sensitivity),
      xmin = 0,
      gama = 0.01975
    )

    # "Desperdicio de agua"
    vf_Desp_A <-
      sapply(
        study_data$resident_potable_water_waste_perception,
        FUN = Value_Function_cut_offs,
        xcuts = c(0.5, 0.75, 0.875, 0.937),
        ycuts = c(1, 0.8, 0.6, 0.4, 0.2),
        xmax = max(
          study_data$resident_potable_water_waste_perception,
          na.rm = T
        )
      )
    # fugas
    fv_fugas <-
      sapply(
        study_data$household_potable_system_lacking_percent,
        FUN = Value_Function_cut_offs,
        xcuts = c(0.5, 0.75, 0.875, 0.937),
        ycuts = c(1, 0.8, 0.6, 0.4, 0.2),
        xmax = max(
          study_data$household_potable_system_lacking_percent,
          na.rm = T
        )
      )

    # falta infrastructura drenaje
    fv_falta <-
      sapply(
        100 * study_data$household_sewer_system_lacking_percent,
        FUN = lack_of_infrastructure_vf,
        x_max = 100 * max(study_data$household_sewer_system_lacking_percent)
      )

    # garbage
    vf_garbage <- sapply(
      study_data$garbage_index,
      FUN = drainages_clogged_vf,
      amplitude = 500000,
      Valor_minimo_Y_en_X = max(study_data$garbage_index)
    )
    # Ponding
    vf_pond <- study_data$ponding_index

    # salud
    vf_H <-
      sapply(
        study_data$resident_diarrhea_per_capita,
        FUN = health_vf,
        max_x = 50,
        saturation = 3
      )

    # house modification flooding
    C_R_D <- cbind(
      vf_WQ,
      vf_UG,
      rep(1, length(fv_falta)),
      rep(1, length(fv_falta)),
      fv_falta,
      vf_garbage,
      rep(1, length(fv_falta)),
      # scarcity does not affect floding
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
      rep(1, length(fv_falta)),
      # flooding do not influence protests or water capture
      vf_H
    )

    distance_ideal_House_mod_lluvia <-
      sweep(
        x=as.matrix(C_R_D[,-c(3, 4, 7)]),
        MARGIN = 2,
        criteria_iz[-c(3, 4, 7)] / sum(criteria_iz[-c(3, 4, 7)]),
        FUN = ideal_distance,
        alternative_weights = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])
      ) # "House modification"
    distance_ideal_House_mod_agua <-
      sweep(
        x=as.matrix(C_R_HM[,-c(6, 8)]),
        MARGIN = 2,
        criteria_iz[-c(6, 8)] / sum(criteria_iz[-c(6, 8)]),
        FUN = ideal_distance,
        alternative_weights = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])
      ) # "House modification"

    list(
      distance_ideal_House_mod_lluvia = distance_ideal_House_mod_lluvia,
      distance_ideal_House_mod_agua = distance_ideal_House_mod_agua
    )
  }

resident_fnss_create <-
  function(value_function_config,
           mental_model_strategy,
           resident_action_efficiency_potable,
           resident_action_efficiency_drainage,
           resilience_threshold) {
    config <- list(
      value_function_config = value_function_config,
      mental_model_strategy = mental_model_strategy,
      params = list(
        resident_action_efficiency_potable = resident_action_efficiency_potable,
        resident_action_efficiency_drainage = resident_action_efficiency_drainage,
        resilience_threshold = resilience_threshold
      )
    )
    prepend_class(config, 'resident_fnss')
  }

#' @method call_fnss resident_fnss
call_fnss.resident_fnss <- function(resident_fnss, study_data, step_in_years, ...) {
  mental_models <- mental_model_resident_create(
    resident_limit_strategy = resident_fnss$mental_model_strategy,
    study_data = study_data
  )
  resident_infrastructure_invest(
    study_data = study_data,
    value_function_config = resident_fnss$value_function_config,
    mental_models = mental_models,
    params = resident_fnss$params,
    step_in_years = step_in_years
  )
}

#' Create a decision strategy for residential investment
#' @param study_data a data frame with the spatial units
#' @param value_function_config A set of value function parameters
#' @param mental_models An object of the mental model class
#' @param params a set of parameters associated to residential investments
#' @param step_in_years the number of years the simulation has run for
resident_infrastructure_invest <-
    function(study_data,
           value_function_config,
           mental_models,
           params,
           step_in_years) {
    suitability <- resident_determine_infrastructure_suitability(
      study_data = study_data,
      value_function_config = value_function_config,
      mental_models = mental_models
    )

    # find agebs that will adapt to reduce effects of flooding
    households_adapt_flooding <-
      which(
        suitability$distance_ideal_House_mod_lluvia > suitability$distance_ideal_House_mod_agua
      )
    # find agebs that will adapt to reduce effects of water scarcity
    households_adapt_water_scarcity <-
      which(
        suitability$distance_ideal_House_mod_lluvia < suitability$distance_ideal_House_mod_agua
      )

    study_data %>%
      dplyr::mutate(
        household_sewer_intervention_count := {
          household_sewer_intervention_count[households_adapt_flooding] <-
            household_sewer_intervention_count[households_adapt_flooding] + 1
          household_sewer_intervention_count
        },
        household_sewer_sensitivity := {
          household_sewer_sensitivity[households_adapt_flooding] <-
            1 - (
              household_sewer_intervention_count[households_adapt_flooding] / step_in_years
              )

          household_sewer_sensitivity
        },
        household_potable_water_invention_count := {
          household_potable_water_invention_count[households_adapt_water_scarcity] <-
            household_potable_water_invention_count[households_adapt_water_scarcity] + 1
          household_potable_water_invention_count
        },
        household_potable_water_sensitivity := {
          household_potable_water_sensitivity[households_adapt_water_scarcity] <-
            1 - (
              household_potable_water_invention_count[households_adapt_water_scarcity] / step_in_years
            )
          household_potable_water_sensitivity
        },
        household_water_storage_tank_percent := {
          household_water_storage_tank_percent[households_adapt_water_scarcity] <-
          household_water_storage_tank_percent[households_adapt_water_scarcity] + (params$resident_action_efficiency_potable / 10) # this menas that for an resident action efficiency of 1, in one step 10% of residents get a new tank (discuss it with the team!)
          household_water_storage_tank_percent
        },
        household_resilience =ifelse(params$resilience_threshold <= resident_asset_index,
                                                   (1 - (params$resilience_threshold / resident_asset_index)) / (1 - params$resilience_threshold),
                                                   0),
        household_potable_water_vulnerability = ((1 - scarcity_index_sensitivity) ^ (1 - household_potable_water_sensitivity)) ^ (1 + household_resilience),
        household_sewer_vulnerability = ((1- flooding_index) ^ (1 - household_sewer_sensitivity)) ^ (1 + household_resilience)
      ) %>%
      dplyr::select(
        censusblock_id,
        household_sewer_intervention_count,
        household_sewer_sensitivity,
        household_potable_water_invention_count,
        household_potable_water_sensitivity,
        household_water_storage_tank_percent,
        household_potable_water_vulnerability,
        household_sewer_vulnerability
      )
  }
#' The initializilation part of the resident component
#' @param resident_fnss A resident component of the class "resident_fnss".
#' @param study_data A data frame with the spatial units and associated fields.
#' @return a data frame with the new variables and their initial state.
resident_initialize <- function(resident_fnss, study_data) {
  study_data %>%
    dplyr::mutate(
      household_potable_water_invention_count = 0L,
      household_sewer_intervention_count = 0L,
      household_potable_water_sensitivity = 1,
      household_sewer_sensitivity = 1,
      household_potable_water_vulnerability = 1,
      household_sewer_vulnerability = 1
    )
}
