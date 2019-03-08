update_adaptation_and_sensitivity <- function(study_data, resident_actions, params, week_of_year) {
  if (week_of_year != 1) {
    return(study_data)
  }

  HM_LL <- resident_actions$HM_LL
  HM_Agua <- resident_actions$HM_Agua

  # change the level of adaptation to water scarcity based on the decision "house modification"
  # change the sensitivity of the agents to water scarcity after the modification

  study_data %>%
    mutate(
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
      vulnerability_Ab = (sensitivity_Ab * days_wn_water_year) / (1 + ingreso),
      vulnerability_D = (sensitivity_D * encharca) / (1 + ingreso)
    )
}

update_protests <- function(study_data, resident_actions, week_of_year) {
  agebs_que_protestan <- resident_actions$agebs_que_protestan
  if (week_of_year == 1) {
    study_data$social_pressure <- 0
  }
  study_data$social_pressure[agebs_que_protestan] <- study_data$social_pressure + 1
  study_data
}

take_actions_residents <- function(site_suitability, params, week_of_year) {
  distance_ideal_House_mod_lluvia <- site_suitability$distance_ideal_House_mod_lluvia
  distance_ideal_House_mod_agua <- site_suitability$distance_ideal_House_mod_agua
  distance_ideal_protest <- site_suitability$distance_ideal_protest

  # residents decisions
  # find agebs that will adapt to reduce effects of flooding
  HM_LL <- which(distance_ideal_House_mod_lluvia > distance_ideal_House_mod_agua)
  # find agebs that will adapt to reduce effects of water scarcity
  HM_Agua <- which(distance_ideal_House_mod_lluvia < distance_ideal_House_mod_agua)

  # From all census blocks that will adapt to reduce the effect of water scarcity
  # find those that will protest
  agebs_que_protestan <- HM_Agua[which(distance_ideal_protest[HM_Agua] > distance_ideal_House_mod_agua[HM_Agua])]

  resident_actions <- list(agebs_que_protestan = agebs_que_protestan, HM_LL = HM_LL, HM_Agua = HM_Agua)

  study_data %>%
    update_adaptation_and_sensitivity(
      study_data = .,
      resident_actions = resident_actions,
      params = params,
      week_of_year = week_of_year) %>%
    update_protests(
      study_data = .,
      resident_actions = resident_actions,
      week_of_year = week_of_year)
}