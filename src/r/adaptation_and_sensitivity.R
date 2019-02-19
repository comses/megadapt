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
