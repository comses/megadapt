.site_selection_inds <-
  function(study_data, site_selection, choice_name) {
    matching_ids <-
      site_selection$censusblock_id[site_selection$choice_name == choice_name]
    study_data$censusblock_id %in% matching_ids
  }
sacmex_normalized_invesment_suitablility <-
  function(sacmex,
           study_data,
           mental_models) {
    suitability <- sacmex_unnormalized_invesment_suitablility(sacmex,
                                               study_data,
                                               mental_models)


    normalize_list <- function(lista){
      (lista-min(lista))/(max(lista)-min(lista))
    }

    suitability %>% dplyr::mutate(
      non_potable_maintenance = normalize_list(non_potable_maintenance),
      non_potable_new_infrastructure = normalize_list(non_potable_new_infrastructure),
      potable_maintenance = normalize_list(potable_maintenance),
      potable_new_infrastructure = normalize_list(potable_new_infrastructure))

}


sacmex_unnormalized_invesment_suitablility <-
  function(sacmex,
           study_data,
           mental_models) {
    value_function_config <- sacmex$value_function_config


    hydraulic_pressure_failure <-
      value_function_config$hydraulic_pressure_failure

    shortage_age <- value_function_config$shortage_age
    shortage_failures <- value_function_config$shortage_failures
    fv_hydraulic_pressure_failure <- value_function_config$hydraulic_pressure_failure
    fv_infrastructure_age <- value_function_config$infrastructure_age
    fv_fix_failure_days <- value_function_config$fix_failure_days
    fv_critical_zones <- value_function_config$critical_zones
    fv_potable_lacking <- value_function_config$potable_lacking


    ## Common
    # peticiones de delegaciones
    vf_pet_del_d <-
      sapply(study_data$delegation_social_pressure, FUN = Peticion_Delegaciones_vf)

    # presion de medios
    vf_pres_medios <-
      sapply(study_data$media_social_pressure, FUN = pression_medios_vf)
    # Ponding
    vf_pond_maintainance <- value_function(sacmex$ponding_fnss, study_data)
    vf_pond_new_infra <- rep(1,length(study_data$ponding_index))

    # f_infrastructure_age <- function_from(fv_infrastructure_age)
    # vf_Age_potable_maintanance <- f_infrastructure_age(
    #   study_data$potable_water_infrastructure_age,
    #   fv_infrastructure_age
    # )
    vf_Age_potable_maintanance <- sapply(
      study_data$potable_water_infrastructure_age,
      FUN = logistica_invertida,
      center = 40,
      k = 0.1,
      xmax = 100,
      xmin = 0
    )

    vf_Age_potable_new_infra=rep(1,length(study_data$potable_water_infrastructure_age))

    # potable water system capacity
    vf_Cap_Ab <-rep(1,length(study_data$household_potable_system_lacking_percent))

    # d) falla Ab
    vf_falla_dist <- 1 - sapply(
      study_data$household_potable_system_lacking_percent,
      FUN = convexa_creciente,
      gama = shortage_failures$gama,
      xmax = shortage_failures$max,
      xmin = shortage_failures$min
    )

    # falta
    # f_lacking_potable <- function_from(fv_potable_lacking)
    # vf_falta_dist <- f_lacking_potable(
    #   study_data$household_potable_system_lacking_percent,
    #   fv_potable_lacking)
    vf_falta_dist <-
      sapply(
        100 * study_data$household_potable_system_lacking_percent,
        FUN = lack_of_infrastructure_vf,
        saturation = 1,
        x_max = 100
      )


    #  plot(study_data$household_potable_system_lacking_percent,vf_falta_Ab)
    # monto ##!!!#no information about this variable
    vf_monto <- rep(1, length(study_data$censusblock_id))

    # hydraulic pressure
    # f_hid_pressure <- function_from(fv_hydraulic_pressure_failure)
    # vf_hid_pressure <- f_hid_pressure(
    #   study_data$potable_system_pressure,
    #   fv_hydraulic_pressure_failure
    # )
    vf_hid_pressure <- sapply(
      study_data$potable_system_pressure,
      FUN = logistic_vf,
      k = hydraulic_pressure_failure$k,
      center = hydraulic_pressure_failure$center,
      xmax = hydraulic_pressure_failure$max,
      xmin = hydraulic_pressure_failure$min
    )


    # Water quality
    vf_WQ <- 1 - study_data$waterquality_index

    # e)water scarcity
    #  vf_scarcity_sacmex <- sapply(study_data$days_wn_water_year, FUN = scarcity_sacmex_vf) # scarcity_annual is calculated dynamically
    vf_scarcity_sacmex <- study_data$scarcity_index_exposure
    # abastecimiento
    vf_Abaste <-
      sapply(
        study_data$resident_potable_water_lacking_count,
        FUN = Value_Function_cut_offs,
        xmax = max(study_data$resident_potable_water_lacking_count, na.rm = T)
      )

    # social_pressure
    #vf_SP <- sapply(study_data$social_pressure, FUN = social_pressure_vf)

    fv_fail_claim <- sapply(
      study_data$resident_reports_potable_water_failure_count,
      FUN = convexa_creciente,
      gama = .2,
      xmax = max(study_data$resident_reports_potable_water_failure_count),
      xmin = min(study_data$resident_reports_potable_water_failure_count)
    )

    vf_SP <- fv_fail_claim * (1 - study_data$scarcity_index_exposure)

    all_C_potable_mantainance <- cbind(
      vf_Age_potable_maintanance,
      vf_Cap_Ab,
      vf_falla_dist,
      vf_falta_dist,
      vf_monto,
      vf_hid_pressure,
      vf_WQ,
      vf_scarcity_sacmex,
      vf_pond_maintainance,
      vf_Abaste,
      vf_pet_del_d,
      vf_pres_medios,
      vf_SP
    )

    all_C_potable_new_infra <- cbind(
      vf_Age_potable_new_infra,
      vf_Cap_Ab,
      vf_falla_dist,
      vf_falta_dist,
      vf_monto,
      vf_hid_pressure,
      vf_WQ,
      vf_scarcity_sacmex,
      vf_pond_new_infra,
      vf_Abaste,
      vf_pet_del_d,
      vf_pres_medios,
      vf_SP
    )

    ## Storm Water Specific

    subsidence <- value_function_config$subsidence
    sewer_age <- value_function_config$sewer_age

    # garbage
    vf_garbage <- sapply(
      study_data$garbage_index,
      FUN = drainages_clogged_vf,
      amplitude = 500000,
      Valor_minimo_Y_en_X = max(study_data$garbage_index)
    )

    # run-off/escurrimiento
    vf_run_off <- sapply(
      study_data$runoff_volume,
      FUN = convexa_decreciente,
      xmax = 504,
      xmin = 0,
      gama = 0.034
    )


    # subsidance
    vf_subside <- sapply(
      study_data$subsidence_rate_per_year,
      FUN = logistica_invertida,
      k = subsidence$k,
      xmin = subsidence$min,
      xmax = subsidence$max,
      center = subsidence$center
    )

    # rainfall
    vf_rain <-sapply(
      study_data$precipitation_volume,
      FUN = convexa_decreciente,
      xmax =  2072,
      xmin = 333,
      gama = 0.034
    )

    # age infrastructure drainage
    vf_A_D_maintenance <-  sapply(
      study_data$sewer_infrastructure_age,
      FUN = logistica_invertida,
      center = 40,
      k = 0.1,
      xmax = 100,
      xmin = 0
    )

    # age infrastructure drainage new infra
    vf_A_D_new_infrastructure <-  rep(1, length(study_data$sewer_infrastructure_age))

    # drainage capacity
    vf_Cap_D <- sapply(
      study_data$sewer_system_capacity,
      FUN = convexa_creciente,
      #xmax = 2064.34,
      xmax = 237.81,
      xmin = 0,
      #gama = 0.197
      gama = 0.064
    )

    #plot(study_data$sewer_system_capacity,vf_Cap_D)
    # falla D
    #vf_fall_D <- rep(1, length(study_data$household_potable_system_lacking_percent))
    vf_falla_dren <- sapply(
      study_data$resident_reports_sewer_failure_count,
      FUN=logistica_invertida,
      xmin=0,
      xmax=10,
      k=0.1,
      center=5)


    vf_falta_dren <-
      sapply(
        100 * study_data$household_sewer_system_lacking_percent,
        FUN = lack_of_infrastructure_vf,
        x_max = 100,
        saturation = 1
      )
    #  plot(study_data$household_sewer_system_lacking_percent,vf_falta_D)
    # peticiones de usuarions delegacionales
    vf_pet_us_d <- study_data$delegation_social_pressure

    # flooding change for vf method
    vf_flood_mantainance <- value_function(sacmex$flooding_fnss, study_data)
    vf_flood_new_infra <- value_function(sacmex$flooding_fnss, study_data)


    all_C_sewer_mantainance <- cbind(
      vf_garbage,
      vf_run_off,
      vf_subside,
      vf_rain,
      vf_A_D_maintenance,
      vf_Cap_D,
      vf_falla_dren,
      vf_falta_dren,
      vf_pet_del_d,
      vf_pet_us_d,
      vf_pres_medios,
      vf_pond_maintainance,
      vf_flood_mantainance
    )

    all_C_sewer_new_infra <- cbind(
      vf_garbage,
      vf_run_off,
      vf_subside,
      vf_rain,
      vf_A_D_new_infrastructure,
      vf_Cap_D,
      vf_falla_dren,
      vf_falta_dren,
      vf_pet_del_d,
      vf_pet_us_d,
      vf_pres_medios,
      vf_pond_new_infra,
      vf_flood_new_infra
    )

    # calculate distance for each census block for action mantainance and build new infrastructure
    sacmcx_criteria_d <- as.vector(mental_models$sacmcx$criteria$d)
    sacmcx_alternative_weights_d <-
      mental_models$sacmcx$alternative_weights$d

    if (dim(all_C_sewer_mantainance)[2] != length(sacmcx_criteria_d) | dim(all_C_sewer_new_infra)[2] != length(sacmcx_criteria_d) ) {
      stop(
        "The number of value functions should be the same length as the number of criteria in the mental model"
      )
    }

    distance_ideal_A1_D <- sweep(
      x=as.matrix(all_C_sewer_mantainance),
      MARGIN = 2,
      sacmcx_criteria_d / sum(sacmcx_criteria_d),
      FUN = ideal_distance,
      alternative_weights = sacmcx_alternative_weights_d[1] / sum(sacmcx_alternative_weights_d)
    ) # "Mantenimiento"
    distance_ideal_A2_D <- sweep(
      x=as.matrix(all_C_sewer_new_infra),
      MARGIN = 2,
      sacmcx_criteria_d / sum(sacmcx_criteria_d),
      FUN = ideal_distance,
      alternative_weights = sacmcx_alternative_weights_d[2] / sum(sacmcx_alternative_weights_d)
    ) # "Nueva_infraestructura"

    sacmcx_criteria_ab <-
      as.vector(mental_models$sacmcx$criteria$ab)
    sacmcx_alternative_weights_s <-
      mental_models$sacmcx$alternative_weights$s

    if (dim(all_C_potable_mantainance)[2] != length(sacmcx_criteria_ab) | dim(all_C_potable_new_infra)[2] != length(sacmcx_criteria_ab)) {
      stop(
        "The number of value functions should be the same length as the number of criteria in the mental model"
      )
    }

    distance_ideal_A1_Ab <- sweep(
      x=as.matrix(all_C_potable_mantainance),
      MARGIN = 2,
      sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
      FUN = ideal_distance,
      alternative_weights = sacmcx_alternative_weights_s['Mantenimiento'] / sum(sacmcx_alternative_weights_s)
    ) # "Mantenimiento"
    distance_ideal_A2_Ab <- sweep(
      x=as.matrix(all_C_potable_new_infra),
      MARGIN = 2,
      sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
      FUN = ideal_distance,
      alternative_weights = sacmcx_alternative_weights_s['Nueva_infraestructura'] / sum(sacmcx_alternative_weights_s)
    ) # "Nueva_infraestructura"

    tibble::tibble(
      censusblock_id = study_data$censusblock_id,
      non_potable_maintenance = distance_ideal_A1_D,
      non_potable_new_infrastructure = distance_ideal_A2_D,
      potable_maintenance = distance_ideal_A1_Ab,
      potable_new_infrastructure = distance_ideal_A2_Ab
    )



  }

#######################################################################################


sacmex_work_plan_max_chooser <-
  function(site_suitability, budget) {
    r <- site_suitability %>% dplyr::select(-censusblock_id)
    choice_names <- colnames(r)
    n_census_blocks <- min(budget, nrow(r))
    choice_name <- choice_names[max.col(as.matrix(r))]
    max_choice_value <- apply(as.matrix(r), 1, max)

    ordered_best_choices <- site_suitability %>%
      dplyr::select(censusblock_id) %>%
      dplyr::mutate(
        choice_name = !!choice_name,
        max_choice_value = !!max_choice_value
      ) %>%
      dplyr::arrange(-max_choice_value)

    ordered_best_choices[0:n_census_blocks, ]
  }

sacmex_work_plan_sewer_potable_split <-
  function(site_suitability,
           budget) {
    # potable_water_budget <- budget$potable_water
    # sewer_budget <- budget$sewer
    # non_potable_water_site_suitability <- site_suitability %>%
    #   dplyr::select(censusblock_id,
    #                 non_potable_maintenance,
    #                 non_potable_new_infrastructure)
    # n_non_potable_water_census_blocks = min(sewer_budget, nrow(site_suitability))
    # non_potable_infrastructure_plan <-
    #   sacmex_work_plan_max_chooser(non_potable_water_site_suitability,
    #                                sewer_budget)
    #
    # potable_water_site_suitability <- site_suitability %>%
    #   dplyr::select(censusblock_id,
    #                 potable_maintenance,
    #                 potable_new_infrastructure)
    # n_potable_water_census_blocks = min(potable_water_budget, nrow(site_suitability))
    # potable_infrastructure_plan <-
    #   sacmex_work_plan_max_chooser(potable_water_site_suitability, potable_water_budget)
    #
    # dplyr::bind_rows(non_potable_infrastructure_plan,
    #                  potable_infrastructure_plan)
    budget1 <- budget$sewer + budget$potable_water
    sacmex_work_plan_max_chooser(site_suitability,budget1)


  }


#' A function to return a work plant for agent sacmex to make infrastructure investments assuming separate budget for each action
#' @param site_suitability A data frame with values of the distance metric for each action
#' @param budget An amount of resources expresed as a number of spatial unit for sacmex to make infrastructure investments
#' @return A data frame with the selected spatial units to invest in the different actions.
sacmex_work_plan_separate_action_budgets <-
  function(site_suitability,
           budget) {
    potable_water_new_infrastructure_budget <- budget$potable_water_new_infrastructure
    potable_water_maintenance_budget <- budget$potable_water_maintenance
    sewer_water_new_infrastructure_budget <- budget$sewer_water_new_infrastructure
    sewer_water_maintenance_budget <- budget$sewer_water_maintenance

    max_budget <- nrow(site_suitability)
    potable_water_new_infrastructure_budget <-
      min(max_budget, potable_water_new_infrastructure_budget)
    potable_water_maintenance_budget <-
      min(max_budget, potable_water_maintenance_budget)
    sewer_water_new_infrastructure_budget <-
      min(max_budget, sewer_water_new_infrastructure_budget)
    sewer_water_maintenance_budget <-
      min(max_budget, sewer_water_maintenance_budget)

    top_vals <- function(df, col_name, budget) {
      df <- df %>%
        dplyr::rename(max_choice_value = !!col_name) %>%
        dplyr::select(censusblock_id, max_choice_value) %>%
        dplyr::arrange(-max_choice_value) %>%
        .[0:budget,]
      if (nrow(df) > 0) {
        df$choice_name = col_name
      }
      df
    }

    potable_water_maintenance_selection <-
      top_vals(site_suitability,
               'potable_maintenance',
               potable_water_maintenance_budget)
    potable_water_new_infrastructure_selection <-
      top_vals(
        site_suitability,
        'potable_new_infrastructure',
        potable_water_new_infrastructure_budget
      )

    sewer_water_maintenance_selection <- top_vals(site_suitability,
                                                  'non_potable_maintenance',
                                                  sewer_water_maintenance_budget)
    sewer_water_new_infrastructure_selection <- top_vals(
      site_suitability,
      'non_potable_new_infrastructure',
      sewer_water_new_infrastructure_budget
    )

    dplyr::bind_rows(
      potable_water_maintenance_selection,
      potable_water_new_infrastructure_selection,
      sewer_water_maintenance_selection,
      sewer_water_new_infrastructure_selection
    )
  }

#' This function is used to implement an investment plan in a single year
#' @param study_data A data frame with the spatial units and associated fields.
#' @param site_selection A work plan
#' @param params A list with parameter values and their names
#' @return The study_data updated with the changes made by sacmex using the work plan
sacmex_implement_work_plan <-
  function(study_data, site_selection, params) {
    A1 <-
      .site_selection_inds(study_data, site_selection, "non_potable_maintenance")
    A2 <-
      .site_selection_inds(study_data,
                           site_selection,
                           "non_potable_new_infrastructure")
    A3 <-
      .site_selection_inds(study_data, site_selection, "potable_maintenance")
    A4 <-
      .site_selection_inds(study_data, site_selection, "potable_new_infrastructure")

    if (length(A1) > 0) {
      study_data$sewer_infrastructure_age[A1] <-
        study_data$sewer_infrastructure_age[A1] - study_data$sewer_infrastructure_age[A1] * params$maintenance_effectiveness_rate

      study_data$sewer_system_capacity[A1] <- pmin(
        study_data$sewer_system_capacity[A1] * (1 + params$maintenance_effectiveness_rate),
        study_data$sewer_system_capacity_max[A1]
      )
      study_data$sacmex_sewer_maintenance_intervention_presence <- A1
    }

    # action 2 New infra D
    if (length(A2) > 0) {
      study_data$household_sewer_system_lacking_percent[A2] <-
        study_data$household_sewer_system_lacking_percent[A2] - study_data$household_sewer_system_lacking_percent[A2] * params$new_infrastructure_effectiveness_rate
      study_data$sewer_system_capacity[A2] <-
        study_data$sewer_system_capacity[A2] * (1 + params$new_infrastructure_effectiveness_rate) # capasity of drainage increases with new infrastructure

      study_data$sacmex_sewer_new_infrastructure_intervention_presence <- A2
    }

    #
    # action 3 mantainance Ab.
    if (length(A3) > 0) {
      study_data$potable_water_infrastructure_age[A3] <-
        study_data$potable_water_infrastructure_age[A3] * (1 - params$maintenance_effectiveness_rate)
      study_data$potable_system_pressure[A3] <- pmin(
        study_data$potable_system_pressure[A3] * (1 + params$maintenance_effectiveness_rate),
        study_data$potable_system_pressure_max[A3]
      )
      study_data$sacmex_potable_maintenance_intervention_presence <- A3
    }

    # action 4 New infra Ab.
    if (length(A4) > 0) {
      study_data$household_potable_system_lacking_percent[A4] <-
        study_data$household_potable_system_lacking_percent[A4] * (1 - params$new_infrastructure_effectiveness_rate)
      study_data$sacmex_potable_new_infrastructure_intervention_presence <- A4
    }
    study_data
  }

sacmex_depreciate_infrastructure <-
  function(study_data, infrastructure_decay_rate) {
    study_data$sewer_infrastructure_age <-
      study_data$sewer_infrastructure_age + 1
    study_data$potable_water_infrastructure_age <-
      study_data$potable_water_infrastructure_age + 1

    study_data$potable_system_pressure <-
      study_data$potable_system_pressure * (1 - infrastructure_decay_rate)
    # update_capacity of the system
    study_data$sewer_system_capacity <-
      study_data$sewer_system_capacity * (1 - infrastructure_decay_rate)

    study_data
  }

## Action Budget Specific

sacmex_get_budget_from_mental_model <-
  function(mental_models,
           sewer_budget,
           potable_water_budget) {
    sewer_weights <-
      mental_models$alternative_weights$d[c('Mantenimiento', 'Nueva_infraestructura')]
    sewer_weights <- sewer_weights / sum(sewer_weights)

    potable_weights <- mental_models$alternative_weights$s
    potable_weights <- potable_weights / sum(potable_weights)

    sewer_split_budget <- sewer_budget * sewer_weights
    potable_split_budget <- potable_water_budget * potable_weights

    list(
      potable_water_new_infrastructure = potable_split_budget[['Nueva_infraestructura']],
      potable_water_maintenance = potable_split_budget[['Mantenimiento']],
      sewer_water_new_infrastructure = sewer_split_budget[['Nueva_infraestructura']],
      sewer_water_maintenance = sewer_split_budget[['Mantenimiento']]
    )
  }

#' Create a sacmex model
#'
#' @param class_name name of S3 class to create
#' @param value_function_config parameterization for value functions
#' @param sewer_mental_model_strategy strategy used to update the sewer mental model
#' @param potable_water_mental_model_strategy strategy used to update the potable water mental model
#' @param sewer_budget the number census blocks a sewer infrastructure improvements can be made on
#' @param potable_water_budget the number census blocks a potable water infrastructure improvements can be made on
#' @param params configuration list determining the effectiveness of maintenance and new infrastructure investment
#' @param flooding_fnss a flooding model with a value function method
#' @param ponding_fnss a ponding model with a value function method
sacmex_default_create <-
  function(value_function_config,
           sewer_mental_model_strategy,
           potable_water_mental_model_strategy,
           sewer_budget,
           potable_water_budget,
           ponding_fnss,
           flooding_fnss,
           class_name,
           params = list(
             maintenance_effectiveness_rate = 0.07,
             new_infrastructure_effectiveness_rate = 0.07
           ),
           distance_mode) {
    config <- list(
      params = params,
      sewer_budget = sewer_budget,
      potable_water_budget = potable_water_budget,
      value_function_config = value_function_config,
      sewer_mental_model_strategy = sewer_mental_model_strategy,
      potable_water_mental_model_strategy = potable_water_mental_model_strategy,
      ponding_fnss = ponding_fnss,
      flooding_fnss = flooding_fnss,
      distance_mode = distance_mode
    )
    prepend_class(config, class_name)
  }

#' Create a sacmex model with split action budgets
#'
#' @export
#' @inheritParams sacmex_default_create
sacmex_seperate_action_budgets_fnss_create <-
  function(value_function_config,
           sewer_mental_model_strategy,
           potable_water_mental_model_strategy,
           sewer_budget,
           potable_water_budget,
           flooding_fnss,
           ponding_fnss,
           params = list(
             maintenance_effectiveness_rate = 0.07,
             new_infrastructure_effectiveness_rate = 0.07
           ),
           distance_mode) {
    sacmex_default_create(
      class_name = 'sacmex_seperate_action_budgets_fnss',
      flooding_fnss = flooding_fnss,
      ponding_fnss = ponding_fnss,
      params = params,
      potable_water_budget = potable_water_budget,
      potable_water_mental_model_strategy = potable_water_mental_model_strategy,
      sewer_budget = sewer_budget,
      sewer_mental_model_strategy = sewer_mental_model_strategy,
      value_function_config = value_function_config,
      distance_mode = distance_mode
    )
  }

sacmex_invest_and_depreciate <-
  function(sacmex,
           year,
           study_data,
           create_budget,
           create_work_plan) {
    mental_models <- list(
      sacmcx =
        mental_model_sacmex_create(
          potable_water_sacmex_limit_strategy = sacmex$potable_water_mental_model_strategy,
          sewer_water_sacmex_limit_strategy = sacmex$sewer_mental_model_strategy,
          year = year,
          study_data = study_data
        )
    )

    params <- sacmex$params
    site_suitability <- sacmex$distance_mode(
      sacmex = sacmex,
      study_data = study_data,
      mental_models = mental_models
    )
    #extended_study_data <- inner_join(study_data,site_suitability, by = PK_JOIN_EXPR)
    #study_data <- apply_data_changes(study_data,site_suitability, join_columns = PK_JOIN_EXPR)

    budget <- create_budget(
      mental_models = mental_models$sacmcx,
      sewer_budget = sacmex$sewer_budget,
      potable_water_budget = sacmex$potable_water_budget
    )

    work_plan <- create_work_plan(
      site_suitability = site_suitability,
      budget = budget
    )
    study_data %>%
      sacmex_implement_work_plan(study_data = .,
                                 site_selection = work_plan,
                                 params = params) %>%
      sacmex_depreciate_infrastructure(
        study_data = .,
        infrastructure_decay_rate = params$infrastructure_decay_rate
      ) %>%
      dplyr::select(
        censusblock_id,
        potable_water_infrastructure_age,
        sewer_infrastructure_age,
        household_sewer_system_lacking_percent,
        sacmex_potable_maintenance_intervention_presence,
        sacmex_sewer_maintenance_intervention_presence,
        sacmex_potable_new_infrastructure_intervention_presence,
        sacmex_sewer_new_infrastructure_intervention_presence,
        sewer_system_capacity,
        household_potable_system_lacking_percent
      ) %>% dplyr::inner_join(site_suitability, by = c("censusblock_id"="censusblock_id"))

  }

#' Create a split action budget sacmex model
#'
#' @export
#' @inheritParams call_fnss
#' @param year current simulation year
#' @param study_data study area tibble
call_fnss.sacmex_seperate_action_budgets_fnss <-
  function(fnss, year, study_data, ...) {
    sacmex_invest_and_depreciate(sacmex = fnss,
                                 year = year,
                                 study_data = study_data,
                                 create_budget = sacmex_get_budget_from_mental_model,
                                 create_work_plan = sacmex_work_plan_separate_action_budgets)
  }

#' Create a potable / sewer split budget sacmex model
#'
#' @export
#' @inheritParams sacmex_default_create
sacmex_fnss_create <-
  function(value_function_config,
           sewer_mental_model_strategy,
           potable_water_mental_model_strategy,
           sewer_budget,
           potable_water_budget,
           params = list(
             maintenance_effectiveness_rate = 0.07,
             new_infrastructure_effectiveness_rate = 0.07
           ),
           flooding_fnss,
           ponding_fnss,
           distance_mode) {
    sacmex_default_create(
      class_name = 'sacmex_fnss',
      flooding_fnss = flooding_fnss,
      ponding_fnss = ponding_fnss,
      params = params,
      potable_water_budget = potable_water_budget,
      potable_water_mental_model_strategy = potable_water_mental_model_strategy,
      sewer_budget = sewer_budget,
      sewer_mental_model_strategy = sewer_mental_model_strategy,
      value_function_config = value_function_config,
      distance_mode = distance_mode
    )
  }

sacmex_get_budget_identity <- function(mental_models,
                                       sewer_budget,
                                       potable_water_budget) {
  list(sewer = sewer_budget,
       potable_water = potable_water_budget)
}

#' Invest in municipality
#'
#' @export
#' @method call_fnss sacmex_fnss
#' @inheritParams call_fnss
#' @param year current year of simulation
#' @param study_data census block data
#' @return A data.frame with municipal infrastructure investment decisions and their impact
#' on the infrastructure
call_fnss.sacmex_fnss <- function(fnss, year, study_data, ...) {
  sacmex_invest_and_depreciate(sacmex = fnss,
                               year = year,
                               study_data = study_data,
                               create_budget = sacmex_get_budget_identity,
                               create_work_plan = sacmex_work_plan_sewer_potable_split)
}

sacmex_deserialize <- function(
  config,
  value_function_config,
  sewer_mental_model_strategy,
  potable_water_mental_model_strategy,
  flooding_fnss,
  ponding_fnss) {

  config <- do.call(sacmex_config_create, config)


  dist_env <- new.env(parent = emptyenv())
  dist_env$normalized <- sacmex_normalized_invesment_suitablility
  dist_env$unnormalized <- sacmex_unnormalized_invesment_suitablility

  distance_mode <- get(config$distance_mode, envir = dist_env)



  env <- new.env(parent = emptyenv())
  env$action_budget <- sacmex_seperate_action_budgets_fnss_create
  env$system_budget <- sacmex_fnss_create

  constructor <- get(config$constructor, envir = env)

  constructor(
    distance_mode = distance_mode,
    value_function_config = value_function_config,
    sewer_mental_model_strategy = sewer_mental_model_strategy,
    potable_water_mental_model_strategy = potable_water_mental_model_strategy,
    sewer_budget = round(config$budget/2),
    potable_water_budget = round(config$budget/2),
    params = list(
      maintenance_effectiveness_rate = config$maintenance_effectiveness_rate,
      new_infrastructure_effectiveness_rate = config$new_infrastructure_effectiveness_rate,
      infrastructure_decay_rate = config$infrastructure_decay_rate
    ),
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss
  )
}

sacmex_config_create <- function(
  constructor = 'system_budget',
  budget = 2000,
  maintenance_effectiveness_rate = 0.1,
  new_infrastructure_effectiveness_rate = 0.1,
  infrastructure_decay_rate = 0.02,
  distance_mode = "unnormalized"
) {
  constructor <- intersect(constructor, c('action_budget', 'system_budget'))
  list(
    constructor = constructor,
    budget = budget,
    distance_mode = distance_mode,
    maintenance_effectiveness_rate = maintenance_effectiveness_rate,
    new_infrastructure_effectiveness_rate = new_infrastructure_effectiveness_rate,
    infrastructure_decay_rate = infrastructure_decay_rate
  )
}


#' Sacmex component initializer
#'
#' @param study_data A data frame with the spatial units and associated fields.
#' @return a data frame with the new variables and their initial state.
sacmex_initialize <- function(study_data) {

  study_data %>%
    dplyr::mutate(
      sewer_infrastructure_age = infrastructure_age,
      potable_water_infrastructure_age = infrastructure_age,
      sacmex_potable_maintenance_intervention_presence = FALSE,
      sacmex_potable_new_infrastructure_intervention_presence = FALSE,
      sacmex_sewer_maintenance_intervention_presence = FALSE,
      sacmex_sewer_new_infrastructure_intervention_presence = FALSE,
      sewer_system_capacity = 0.5 * sewer_system_capacity_max,
      potable_system_pressure = 0.5 * potable_system_pressure_max,
      non_potable_maintenance = 0,
      non_potable_new_infrastructure = 0,
      potable_maintenance = 0,
      potable_new_infrastructure = 0
    )
}
