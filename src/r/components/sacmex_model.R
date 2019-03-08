determine_site_selection <- function(site_suitability, budget) {
  # first find the ranking of non-dominant solutions in the pareto frontier
  r <-
    rbind(
      site_suitability$distance_ideal_A1_D,
      site_suitability$distance_ideal_A2_D,
      site_suitability$distance_ideal_A1_Ab,
      site_suitability$distance_ideal_A2_Ab
    )

  choices <- max.col(as.matrix(r))

  # save ID of selected agebs
  selected_agebs <- order(r)[1:min(budget, length(r))]

  # Store ID of agebs that will be modified by sacmex
  A1 <- selected_agebs[which(choices == 1)] # "Mantenimiento" D
  A2 <- selected_agebs[which(choices == 2)] # "Nueva_infraestructura" D
  A3 <- selected_agebs[which(choices == 3)] # "Mantenimiento" Ab
  A4 <- selected_agebs[which(choices == 4)] # "Nueva_infraestructura" Ab

  list(
    A1 = A1,
    A2 = A2,
    A3 = A3,
    A4 = A4
  )
}

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

  study_data %>%
    update_infrastructure_age(
      study_data=.,
      infrastructure_decay_rate = params$infrastructure_decay_rate) %>%
    select(ageb_id,
           antiguedad_Ab,
           antiguedad_D,
           bombeo_tot,
           falta_dren,
           Interventions_Ab,
           Interventions_D,
           q100,
           V_SAGUA)
}

update_infrastructure_age <- function(study_data, infrastructure_decay_rate) {
  # update infrastructure related atributes
  # update_age_infrastructure
  study_data$antiguedad_D <- study_data$antiguedad_D + 1
  study_data$antiguedad_Ab <- study_data$antiguedad_Ab + 1

  # update_capacity of the system
  study_data$capac_w <- study_data$capac_w * (1 - infrastructure_decay_rate)
  # update capacity index
  # FIDEL
  # The proportion of people without infrastructure increases proportionally to
  # the growthof the population in each delegation
  study_data$FALTA_IN <- study_data$FALTA_IN * (1 + (1 - study_data$FALTA_IN) * study_data$pop_growth)
  study_data$falta_dren <- study_data$falta_dren * (1 + (1 - study_data$falta_dren) * study_data$pop_growth)

  study_data
}
