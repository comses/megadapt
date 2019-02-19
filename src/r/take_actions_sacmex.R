take_actions_sacmex <- function(study_data, site_selection, params) {
  A1 <- site_selection$A1
  A2 <- site_selection$A2
  A3 <- site_selection$A3
  A4 <- site_selection$A4

  # take actions sacmex
  # cahnge value of atributes in agebs selected for action
  # action 1 mantainance D
  if (length(A1) > 0) {
    study_data$antiguedad_D[A1] <- study_data$antiguedad_D[A1] - study_data$antiguedad_D[A1] * params$maintenance_effectiveness_rate
    study_data$q100[A1] <- study_data$q100[A1] * (1 + params$maintenance_effectiveness_rate) # capasity of drainage increases with mantainance
    study_data$Interventions_D[A1] <- study_data$Interventions_D[A1] + 1
  }

  # action 2 New infra D
  if (length(A2) > 0) {
    study_data$falta_dren[A2] <- study_data$falta_dren[A2] - study_data$falta_dren[A2] * params$new_infrastructure_effectiveness_rate
    study_data$q100[A2] <- study_data$q100[A2] * (1 + params$maintenance_effectiveness_rate) # capasity of drainage increases with new infrastructure
    study_data$bombeo_tot[A2] <- study_data$bombeo_tot[A2] + 1 # capasity of drainage increases with new infrastructure

    study_data$Interventions_D[A2] <- study_data$Interventions_D[A2] + 1
  }

  # action 3 mantainance Ab.
  if (length(A3) > 0) {
    study_data$antiguedad_Ab[A3] <- study_data$antiguedad_Ab[A3] * (1 - params$maintenance_effectiveness_rate)
    study_data$Interventions_Ab[A3] <- study_data$Interventions_Ab[A3] + 1
  }

  # action 4 New infra Ab.
  if (length(A4) > 0) {
    study_data$V_SAGUA[A4] <- study_data$V_SAGUA[A4] * (1 - params$new_infrastructure_effectiveness_rate)
    study_data$Interventions_Ab[A4] <- study_data$Interventions_Ab[A4] + 1
  }

  study_data
}
