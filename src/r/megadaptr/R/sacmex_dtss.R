.site_selection_inds <-
  function(study_data, site_selection, choice_name) {
    matching_ids <-
      site_selection$censusblock_id[site_selection$choice_name == choice_name]
    study_data$censusblock_id %in% matching_ids
  }

sacmex_determine_investment_suitability <-
  function(study_data,
           value_function_config,
           mental_models) {
    shortage_age <- value_function_config$shortage_age
    shortage_failures <- value_function_config$shortage_failures
    hydraulic_pressure_failure <-
      value_function_config$hydraulic_pressure_failure

    ## Common
    # peticiones de delegaciones
    vf_pet_del_d <-
      sapply(study_data$delegation_social_pressure, FUN = Peticion_Delegaciones_vf)

    # presion de medios
    vf_pres_medios <-
      sapply(study_data$media_social_pressure, FUN = pression_medios_vf)
    # flooding #cchange to flooding
    vf_flood <- sapply(study_data$resident_reports_flooding_per_year, FUN = ponding_vf)

    # Ponding
    vf_pond_maintainance <- study_data$ponding_index
    vf_pond_new_infra <- rep(1,length(study_data$ponding_index))
    ## Fresh Water Specific
    # age infrastructure Abastecimiento

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
    vf_Cap_Ab <- rep(1, length(study_data$household_potable_system_lacking_percent))

    # d) falla Ab
    vf_falla_dist <- 1 - sapply(
      study_data$household_potable_system_lacking_percent,
      FUN = convexa_creciente,
      gama = shortage_failures$gama,
      xmax = shortage_failures$max,
      xmin = shortage_failures$min
    )

    # falta
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
    vf_hid_pressure <- sapply(
      study_data$potable_system_pressure,
      FUN = logistic_vf,
      k = hydraulic_pressure_failure$k,
      center = hydraulic_pressure_failure$center,
      xmax = hydraulic_pressure_failure$max,
      xmin = hydraulic_pressure_failure$min
    )

    # Water quality
    vf_WQ <-
      sapply(study_data$waterquality_index, FUN = water_quality_residents_vf)

    # e)water scarcity
    #  vf_scarcity_sacmex <- sapply(study_data$days_wn_water_year, FUN = scarcity_sacmex_vf) # scarcity_annual is calculated dynamically
    vf_scarcity_sacmex <- study_data$scarcity_index
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

    vf_SP <- fv_fail_claim * (1 - study_data$scarcity_index)

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
      vf_pond,
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
    vf_run_off <- sapply(study_data$runoff_volume, FUN = run_off_vf)

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
    vf_rain <- sapply(study_data$precipitation_volume, FUN = rainfall_vf)

    # age infrastructure drainage
    vf_A_D <- sapply(
      study_data$sewer_infrastructure_age,
      FUN = logistica_invertida,
      center = sewer_age$center,
      k = sewer_age$k,
      xmax = sewer_age$max,
      xmin = sewer_age$min
    )

    # drainage capacity
    vf_Cap_D <-
      sapply(
        study_data$non_potable_capacity,
        FUN = capacity_drainage_vf,
        sat = 1,
        x_max = max(study_data$non_potable_capacity),
        x_min = 0
      )
    #plot(study_data$non_potable_capacity,vf_Cap_D)
    # falla D
    #vf_fall_D <- rep(1, length(study_data$household_potable_system_lacking_percent))
    vf_falla_dren <- sapply(
      study_data$resident_reports_sewer_failure_count,
      FUN=logistic_vf,
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
    vf_pet_us_d <-
      sapply(
        study_data$delegation_social_pressure,
        FUN = Peticiones_usuarios_vf,
        xmax = max(study_data$delegation_social_pressure, na.rm = T)
      )

    # flooding #cchange to flooding
    vf_flood_mantainance <- 1 - study_data$flooding_index
    vf_flood_new_infra <- 1 - study_data$flooding_index

    all_C_sewer_mantainance <- cbind(
      vf_garbage,
      vf_run_off,
      vf_subside,
      vf_rain,
      vf_A_D,
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
      vf_A_D,
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

    ordered_best_choices[0:n_census_blocks,]
  }

sacmex_work_plan_sewer_potable_split <-
  function(site_suitability,
           potable_water_budget,
           non_potable_water_budget) {
    non_potable_water_site_suitability <- site_suitability %>%
      dplyr::select(censusblock_id,
                    non_potable_maintenance,
                    non_potable_new_infrastructure)
    n_non_potable_water_census_blocks = min(non_potable_water_budget, nrow(site_suitability))
    non_potable_infrastructure_plan <-
      determine_public_infrastructure_work_plan(non_potable_water_site_suitability,
                                                non_potable_water_budget)

    potable_water_site_suitability <- site_suitability %>%
      dplyr::select(censusblock_id, potable_maintenance, potable_new_infrastructure)
    n_potable_water_census_blocks = min(potable_water_budget, nrow(site_suitability))
    potable_infrastructure_plan <-
      sacmex_work_plan(potable_water_site_suitability, potable_water_budget)

    dplyr::bind_rows(non_potable_infrastructure_plan,
                     potable_infrastructure_plan)
  }

sacmex_work_plan_separate_action_budgets <-
  function(site_suitability,
           potable_water_new_infrastructure_budget,
           potable_water_maintenance_budget,
           sewer_water_new_infrastructure_budget,
           sewer_water_maintenance_budget) {
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
        .[0:budget, ]
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

    # take actions sacmex
    # change value of atributes in agebs selected for action
    # action 1 mantainance D
    #The effect of mantainance will not surpass the max. non_potable_capacity for each ageb!!!
    if (length(A1) > 0) {
      study_data$sewer_infrastructure_age[A1] <-
        study_data$sewer_infrastructure_age[A1] - study_data$sewer_infrastructure_age[A1] * params$maintenance_effectiveness_rate

      study_data$non_potable_capacity[A1] <- pmin(
        study_data$non_potable_capacity[A1] * (1 + params$maintenance_effectiveness_rate),
        # capasity of drainage increases with mantainance
        study_data$sewer_system_capacity_max[A1]
      )
      study_data$sacmex_sewer_maintenance_intervention_count[A1] <-
        study_data$sacmex_sewer_maintenance_intervention_count[A1] + 1
    }

    # action 2 New infra D
    if (length(A2) > 0) {
      study_data$household_sewer_system_lacking_percent[A2] <-
        study_data$household_sewer_system_lacking_percent[A2] - study_data$household_sewer_system_lacking_percent[A2] * params$new_infrastructure_effectiveness_rate
      study_data$non_potable_capacity[A2] <-
        study_data$non_potable_capacity[A2] * (1 + params$new_infrastructure_effectiveness_rate) # capasity of drainage increases with new infrastructure

      study_data$sacmex_sewer_new_infrastructure_intervention_count[A2] <-
        study_data$sacmex_sewer_new_infrastructure_intervention_count[A2] + 1
    }

    #
    # action 3 mantainance Ab.
    if (length(A3) > 0) {
      study_data$potable_water_infrastructure_age[A3] <-
        study_data$potable_water_infrastructure_age[A3] * (1 - params$maintenance_effectiveness_rate)
      study_data$sacmex_potable_maintenance_intervention_count[A3] <-
        study_data$sacmex_potable_maintenance_intervention_count[A3] + 1
    }

    # action 4 New infra Ab.
    if (length(A4) > 0) {
      study_data$household_potable_system_lacking_percent[A4] <-
        study_data$household_potable_system_lacking_percent[A4] * (1 - params$new_infrastructure_effectiveness_rate)
      study_data$sacmex_potable_new_infrastructure_intervention_count[A4] <-
        study_data$sacmex_potable_new_infrastructure_intervention_count[A4] + 1
    }
    study_data
  }

sacmex_depreciate_infrastructure <-
  function(study_data, infrastructure_decay_rate) {
    study_data$sewer_infrastructure_age <- study_data$sewer_infrastructure_age + 1
    study_data$potable_water_infrastructure_age <- study_data$potable_water_infrastructure_age + 1

    # update_capacity of the system
    study_data$non_potable_capacity <-
      study_data$non_potable_capacity * (1 - infrastructure_decay_rate)
    # update capacity index
    # FIDEL
    # The proportion of people without infrastructure increases proportionally to
    # the growthof the population in each delegation
    #study_data$household_potable_system_lacking_percent <- study_data$household_potable_system_lacking_percent * (1 + (1 - study_data$household_potable_system_lacking_percent)*pop_growth)
    #study_data$household_sewer_system_lacking_percent <- study_data$household_sewer_system_lacking_percent * (1 + (1 - study_data$household_sewer_system_lacking_percent)*pop_growth)

    study_data
  }

## Action Budget Specific

sacmex_get_budget_from_mental_model <- function(mental_models, sewer_budget, potable_water_budget) {
  sewer_weights <- mental_models$alternative_weights$d[c('Mantenimiento', 'Nueva_infraestructura')]
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

sacmex_seperate_action_budgets_fnss_create <-
  function(value_function_config,
           sewer_mental_model_strategy,
           potable_water_mental_model_strategy,
           sewer_budget,
           potable_water_budget,
           params = list(
             maintenance_effectiveness_rate = 0.07,
             new_infrastructure_effectiveness_rate = 0.07
           )) {
    config <- list(
      params = params,
      sewer_budget = sewer_budget,
      potable_water_budget = potable_water_budget,
      value_function_config = value_function_config,
      sewer_mental_model_strategy = sewer_mental_model_strategy,
      potable_water_mental_model_strategy = potable_water_mental_model_strategy
    )
    prepend_class(config, 'sacmex_seperate_action_budgets_fnss')
  }

#' @export
#' @method call_fnss sacmex_seperate_action_budgets_fnss
call_fnss.sacmex_seperate_action_budgets_fnss <-
  function(sacmex, year, study_data) {
    mental_models <- list(
      sacmcx =
        mental_model_sacmex_create(
          potable_water_sacmex_limit_strategy = sacmex$potable_water_mental_model_strategy,
          sewer_water_sacmex_limit_strategy = sacmex$potable_water_mental_model_strategy,
          year = year,
          study_data = study_data
        )
    )
    budget <- sacmex_get_budget_from_mental_model(
      mental_models = mental_models$sacmcx,
      sewer_budget = sacmex$sewer_budget,
      potable_water_budget = sacmex$potable_water_budget
    )

    params <- sacmex$params
    site_suitability <- sacmex_determine_investment_suitability(
      study_data = study_data,
      mental_models = mental_models,
      value_function_config = sacmex$value_function_config
    )
    work_plan <- sacmex_work_plan_separate_action_budgets(
      site_suitability = site_suitability,
      potable_water_new_infrastructure_budget = budget$potable_water_new_infrastructure,
      potable_water_maintenance_budget = budget$potable_water_maintenance,
      sewer_water_new_infrastructure_budget = budget$sewer_water_new_infrastructure,
      sewer_water_maintenance_budget = budget$sewer_water_maintenance
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
        sacmex_potable_maintenance_intervention_count,
        sacmex_sewer_maintenance_intervention_count,
        sacmex_potable_new_infrastructure_intervention_count,
        sacmex_sewer_new_infrastructure_intervention_count,
        non_potable_capacity,
        household_potable_system_lacking_percent
      )
  }

## Regular budget

sacmex_fnss_create <-
  function(value_function_config,
           sewer_mental_model_strategy,
           potable_water_mental_model_strategy,
           sewer_budget,
           potable_water_budget,
           params = list(
             maintenance_effectiveness_rate = 0.07,
             new_infrastructure_effectiveness_rate = 0.07
           )) {
    config <- list(
      params = params,
      sewer_budget = sewer_budget,
      potable_water_budget = potable_water_budget,
      value_function_config = value_function_config,
      sewer_mental_model_strategy = sewer_mental_model_strategy,
      potable_water_mental_model_strategy = potable_water_mental_model_strategy
    )
    prepend_class(config, 'sacmex_fnss')
  }

call_fnss.sacmex_fnss <- function(sacmex, study_data) {

}

## Initialization Specific

sacmex_initialize <- function(study_data) {
  study_data %>%
    dplyr::mutate(
      sewer_infrastructure_age = infrastructure_age,
      potable_water_infrastructure_age = infrastructure_age,
      sacmex_potable_maintenance_intervention_count = 0,
      sacmex_potable_new_infrastructure_intervention_count = 0,
      sacmex_sewer_maintenance_intervention_count = 0,
      sacmex_sewer_new_infrastructure_intervention_count = 0,
      non_potable_capacity = sewer_system_capacity_max
    )
}
