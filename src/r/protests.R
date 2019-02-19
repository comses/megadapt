# Protests

update_protests <- function(study_data, resident_actions, year_changed) {
  agebs_que_protestan <- resident_actions$agebs_que_protestan
  # If the decision is to protest  (in "take_actions_residents.R"),
  # Here the protest is triggered and saved.

  study_data$protesta[agebs_que_protestan] <- 1


  # accumulate protests as social_pressure
  if (year_changed) {
    study_data$social_pressure <- study_data$protesta
  } else {
    study_data$social_pressure <- study_data$social_pressure + study_data$protesta
  }
  study_data
}
