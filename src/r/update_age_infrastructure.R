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
