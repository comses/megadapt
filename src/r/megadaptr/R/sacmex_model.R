determine_public_infrastructure_investment_suitability <- function(study_data, value_function_config, mental_models) {
  shortage_age <- value_function_config$shortage_age
  shortage_failures <- value_function_config$shortage_failures
  hydraulic_pressure_failure <- value_function_config$hydraulic_pressure_failure

  ## Common
  # peticiones de delegaciones
  vf_pet_del_d<- sapply(study_data$pet_del_d, FUN = Peticion_Delegaciones_vf)

  # presion de medios
  vf_pres_medios <- sapply(study_data$pres_med, FUN = pression_medios_vf)
  # flooding #cchange to flooding
  vf_flood <- sapply(study_data$inunda, FUN = ponding_vf)

  # Ponding
  vf_pond <- sapply(study_data$encharca, FUN = ponding_vf)

  ## Fresh Water Specific
  # age infrastructure Abastecimiento
  vf_A_Ab <- sapply(study_data$antiguedad_dist,
                    FUN = campana_invertida,
                    center = shortage_age$center,
                    a = shortage_age$a,
                    xmax = shortage_age$max,
                    xmin = shortage_age$min)

  # potable water system capacity
  vf_Cap_Ab <- rep(1, length(study_data$falta_dist))

  # d) falla Ab
  vf_falla_dist <- 1 - sapply(study_data$falla_dist,
                         FUN = convexa_creciente,
                         gama = shortage_failures$gama,
                         xmax = shortage_failures$max,
                         xmin = shortage_failures$min)

  # falta
  vf_falta_dist <- sapply(100 * study_data$falta_dist, FUN = lack_of_infrastructure_vf,saturation=1,x_max=100)
#  plot(study_data$falta_dist,vf_falta_Ab)
  # monto ##!!!#no information about this variable
  vf_monto <- rep(1, length(study_data$ageb_id))

  # hydraulic pressure
  vf_hid_pressure <- sapply(study_data$pres_hid,
                            FUN = logistic_vf,
                            k = hydraulic_pressure_failure$k,
                            center = hydraulic_pressure_failure$center,
                            xmax = hydraulic_pressure_failure$max,
                            xmin = hydraulic_pressure_failure$min)

  # Water quality
  vf_WQ <- sapply(study_data$cal_agua, FUN = water_quality_residents_vf)

  # e)water scarcity
#  vf_scarcity_sacmex <- sapply(study_data$days_wn_water_year, FUN = scarcity_sacmex_vf) # scarcity_annual is calculated dynamically
  vf_scarcity_sacmex<-study_data$scarcity_index
  # abastecimiento
  vf_Abaste <- sapply(study_data$abastecimi, FUN = Value_Function_cut_offs, xmax = max(study_data$abastecimi, na.rm = T))

  # social_pressure
  #vf_SP <- sapply(study_data$social_pressure, FUN = social_pressure_vf)

  fv_fail_claim <- sapply(study_data$fail_claim,
                        FUN= convexa_creciente,
                        gama=.2,
                        xmax=max(study_data$fail_claim),
                        xmin=min(study_data$fail_claim))

  vf_SP <-fv_fail_claim*(1-study_data$scarcity_index)

  all_C_ab <- cbind(
    vf_A_Ab,
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
  vf_garbage <- sapply(study_data$basura,
                       FUN = drainages_clogged_vf,
                       amplitude = 500000,
                       Valor_minimo_Y_en_X = max(study_data$basura))

  # run-off/escurrimiento
  vf_run_off <- sapply(study_data$f_esc, FUN = run_off_vf)

  # subsidance
  vf_subside <- sapply(study_data$subsidenci,
                       FUN = logistic_invertida,
                       k = subsidence$k,
                       xmin = subsidence$min,
                       xmax = subsidence$max,
                       center = subsidence$center)

  # rainfall
  vf_rain <- sapply(study_data$f_prec_v, FUN = rainfall_vf)

  # age infrastructure drainage
  vf_A_D <- sapply(study_data$antiguedad_dren,
                   FUN = logistic_invertida,
                   center = sewer_age$center,
                   k = sewer_age$k,
                   xmax = sewer_age$max,
                   xmin = sewer_age$min)

  # drainage capacity
  vf_Cap_D<-sapply(study_data$q100,FUN = capacity_drainage_vf,sat=1,x_max=max(study_data$q100),x_min=0)
#plot(study_data$q100,vf_Cap_D)
  # falla D
  #vf_fall_D <- rep(1, length(study_data$falla_dist))
  vf_falla_dren <- 1 - sapply(study_data$falla_dren,FUN = capacity_drainage_vf,sat=1,x_max=max(study_data$falla_dist),x_min=0)
#  plot(study_data$falla_dist,vf_fall_D)
    #falta dren
  vf_falta_dren <- sapply(100 * study_data$falta_dren, FUN = lack_of_infrastructure_vf,x_max=100,saturation=1)
#  plot(study_data$falta_dren,vf_falta_D)
  # peticiones de usuarions delegacionales
  vf_pet_us_d <- sapply(study_data$pet_del_d, FUN = Peticiones_usuarios_vf, xmax = max(study_data$pet_del_d, na.rm = T))

  # flooding #cchange to flooding
  vf_flood <- sapply(study_data$inunda, FUN = ponding_vf)

  all_C_D <- cbind(
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
    vf_pond,
    vf_flood
  )
  # calculate distance for each census block for action mantainance and build new infrastructure
  sacmcx_criteria_d <- as.vector(mental_models$sacmcx$criteria$d)
  sacmcx_alternative_weights_d <- mental_models$sacmcx$alternative_weights$d

if(dim(all_C_D)[2]!=length(sacmcx_criteria_d)){stop("number of value functions defined should be the same length as the number of criteria in the mental model")}

  distance_ideal_A1_D <- sweep(as.matrix(all_C_D),
                               MARGIN = 2,
                               sacmcx_criteria_d / sum(sacmcx_criteria_d),
                               FUN = ideal_distance,
                               z = sacmcx_alternative_weights_d[1] / sum(sacmcx_alternative_weights_d)) # "Mantenimiento"
  distance_ideal_A2_D <- sweep(as.matrix(all_C_D),
                               MARGIN = 2,
                               sacmcx_criteria_d / sum(sacmcx_criteria_d),
                               FUN = ideal_distance,
                               z = sacmcx_alternative_weights_d[2] / sum(sacmcx_alternative_weights_d)) # "Nueva_infraestructura"

  sacmcx_criteria_ab <- as.vector(mental_models$sacmcx$criteria$ab)
  sacmcx_alternative_weights_s <- mental_models$sacmcx$alternative_weights$s

  if(dim(all_C_ab)[2]!=length(sacmcx_criteria_ab)){stop("number of value functions defined should be the same length as the number of criteria in the mental model")}

  distance_ideal_A1_Ab <- sweep(as.matrix(all_C_ab),
                                MARGIN = 2,
                                sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
                                FUN = ideal_distance,
                                z = sacmcx_alternative_weights_s[4] / sum(sacmcx_alternative_weights_s[c(4, 5)])) # "Mantenimiento"
  distance_ideal_A2_Ab <- sweep(as.matrix(all_C_ab),
                                MARGIN = 2,
                                sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
                                FUN = ideal_distance,
                                z = sacmcx_alternative_weights_s[5] / sum(sacmcx_alternative_weights_s[c(4, 5)])) # "Nueva_infraestructura"

  tibble::tibble(
    ageb_id = study_data$ageb_id,
    distance_ideal_A1_D = distance_ideal_A1_D,
    distance_ideal_A2_D = distance_ideal_A2_D,
    distance_ideal_A1_Ab = distance_ideal_A1_Ab,
    distance_ideal_A2_Ab = distance_ideal_A2_Ab
  )
}

determine_public_infrastructure_work_plan <- function(site_suitability, budget) {
  r <- site_suitability
  n_census_blocks <- min(budget, nrow(r))
  choices <- max.col(as.matrix(r))
  choice_values <- apply(as.matrix(r), 1, max)

  # save ID of selected agebs
  selected_agebs <- order(choice_values)[1:min(budget, length(choice_values))]

  # Store ID of agebs that will be modified by sacmex
  A1 <- which(choices == 1) # "Mantenimiento" D
  A2 <- which(choices == 2) # "Nueva_infraestructura" D
  A3 <- which(choices == 3) # "Mantenimiento" Ab
  A4 <- which(choices == 4) # "Nueva_infraestructura" Ab

  tibble::tibble(
    ageb_id = r$ageb_id[selected_agebs],
    choices = choices[selected_agebs]
  ) %>%
    dplyr::mutate(
      A1 = (choices == 1),
      A2 = (choices == 2),
      A3 = (choices == 3),
      A4 = (choices == 4)
    )
}

make_public_infrastructure_investments <- function(study_data, site_selection, params) {
  A1 <- site_selection$A1
  A2 <- site_selection$A2
  A3 <- site_selection$A3
  A4 <- site_selection$A4

  # take actions sacmex
  # change value of atributes in agebs selected for action
  # action 1 mantainance D
  if (length(A1) > 0) {
    study_data$antiguedad_dren[A1] <- study_data$antiguedad_dren[A1] - study_data$antiguedad_dren[A1] * params$maintenance_effectiveness_rate
    study_data$q100[A1] <- study_data$q100[A1] * (1 + params$maintenance_effectiveness_rate) # capasity of drainage increases with mantainance
    study_data$Interventions_D[A1] <- study_data$Interventions_D[A1] + 1
  }

  # action 2 New infra D
  if (length(A2) > 0) {
    study_data$falta_dren[A2] <- study_data$falta_dren[A2] - study_data$falta_dren[A2] * params$new_infrastructure_effectiveness_rate
    study_data$q100[A2] <- study_data$q100[A2] * (1 + params$maintenance_effectiveness_rate) # capasity of drainage increases with new infrastructure

    study_data$Interventions_D[A2] <- study_data$Interventions_D[A2] + 1
  }

  # action 3 mantainance Ab.
  if (length(A3) > 0) {
    study_data$antiguedad_dist[A3] <- study_data$antiguedad_dist[A3] * (1 - params$maintenance_effectiveness_rate)
    study_data$Interventions_Ab[A3] <- study_data$Interventions_Ab[A3] + 1
  }

  # action 4 New infra Ab.
  if (length(A4) > 0) {
    study_data$falta_dist[A4] <- study_data$falta_dist[A4] * (1 - params$new_infrastructure_effectiveness_rate)
    study_data$Interventions_Ab[A4] <- study_data$Interventions_Ab[A4] + 1
  }

  study_data
}

create_public_infrastructure_work_plan <- function(study_data, value_function_config, mental_models, budget) {
  suitability <- determine_public_infrastructure_investment_suitability(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models
  )

  work_plan <- determine_public_infrastructure_work_plan(
    site_suitability = suitability,
    budget = budget
  )

  work_plan
}

depreciate_public_infrastructure <- function(study_data, infrastructure_decay_rate) {
  study_data$antiguedad_dren <- study_data$antiguedad_dren + 1
  study_data$antiguedad_dist <- study_data$antiguedad_dist + 1

  # update_capacity of the system
  study_data$q100 <- study_data$q100 * (1 - infrastructure_decay_rate)
  # update capacity index
  # FIDEL
  # The proportion of people without infrastructure increases proportionally to
  # the growthof the population in each delegation
  #study_data$falta_dist <- study_data$falta_dist * (1 + (1 - study_data$falta_dist)*pop_growth)
  #study_data$falta_dren <- study_data$falta_dren * (1 + (1 - study_data$falta_dren)*pop_growth)

  study_data
}

update_public_infrastructure <- function(study_data, value_function_config, mental_models, params) {
  site_selection <- create_public_infrastructure_work_plan(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    budget = params$budget)
  study_data %>%
    make_public_infrastructure_investments(
      study_data = .,
      site_selection = site_selection,
      params = params) %>%
    depreciate_public_infrastructure(
      study_data = .,
      infrastructure_decay_rate = params$infrastructure_decay_rate) %>%
    dplyr::select(
      ageb_id,
      antiguedad_dist,
      antiguedad_dren,
      falta_dren,
      Interventions_Ab,
      Interventions_D,
      q100,
      falta_dist)
}

sacmex_component <- list(
  initialize = function(study_data) {
    study_data %>%
      mutate(antiguedad_dren = antiguedad,
             antiguedad_dist = antiguedad,
             Interventions_Ab = 0,
             Interventions_D = 0)
  },
  transition = update_public_infrastructure
)
