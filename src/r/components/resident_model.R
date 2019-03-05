take_actions_residents <- function(site_suitability) {
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
  
  list(agebs_que_protestan = agebs_que_protestan, HM_LL = HM_LL, HM_Agua = HM_Agua)
}

update_adaptation_and_sensitivity <- function(study_data, resident_actions, params) {
  HM_LL <- resident_actions$HM_LL
  HM_Agua <- resident_actions$HM_Agua
  
  # change the level of adaptation to water scarcity based on the decision "house modification"
  # change the sensitivity of the agents to water scarcity after the modification
  
  if (length(HM_LL) > 0) {
    study_data$house_modifications_D[HM_LL] <- study_data$house_modifications_D[HM_LL] + 1
    study_data$sensitivity_D[HM_Agua] <- 1 - (study_data$house_modifications_D[HM_Agua] / (params$half_sensitivity_d + study_data$house_modifications_D[HM_Agua]))
  }
  
  # change the level of adaptation to flooding based on the decision "house modification"
  # change the sensitivity of the agents to flooding events after the modification
  
  if (length(HM_Agua) > 0) {
    study_data$house_modifications_Ab[HM_Agua] <- study_data$house_modifications_Ab[HM_Agua] + 1
    study_data$sensitivity_Ab[HM_Agua] <- 1 - (study_data$house_modifications_Ab[HM_Agua] / (params$half_sensitivity_ab + study_data$house_modifications_Ab[HM_Agua]))
  }
  
  # update vulnerability
  study_data$vulnerability_Ab <- (study_data$sensitivity_Ab * study_data$days_wn_water_year) / (1 + study_data$ingreso)
  study_data$vulnerability_D <- (study_data$sensitivity_D * study_data$encharca) / (1 + study_data$ingreso)
  
  study_data
}

update_protests <- function(study_data, resident_actions, n_weeks_in_year) {
  agebs_que_protestan <- resident_actions$agebs_que_protestan
  # If the decision is to protest  (in "take_actions_residents.R"),
  # Here the protest is triggered and saved.
  
  study_data$protesta[agebs_que_protestan] <- 1
  # accumulate protests as social_pressure
  study_data$social_pressure <- n_weeks_in_year*study_data$protesta
  study_data
}
