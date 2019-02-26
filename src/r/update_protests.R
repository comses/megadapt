# Protests

update_protests <- function(study_data, resident_actions, n_weeks_in_year) {
  agebs_que_protestan <- resident_actions$agebs_que_protestan
  # If the decision is to protest  (in "take_actions_residents.R"),
  # Here the protest is triggered and saved.

  study_data$protesta[agebs_que_protestan] <- 1
  # accumulate protests as social_pressure
  study_data$social_pressure <- n_weeks_in_year*study_data$protesta
  study_data
}
