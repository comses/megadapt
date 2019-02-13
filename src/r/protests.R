#Protests

update_protests <- function(study_area_cvg, agebs_que_protestan) {
  #If the decision is to protest  (in "take_actions_residents.R"),
  #Here the protest is triggered and saved.

  study_area_cvg@data$protesta[agebs_que_protestan] <- 1


  #accumulate protests as social_pressure
  if (year_change[i] == 1) {
    study_area_cvg@data$social_pressure <- study_area_cvg@data$protesta
  } else {
    study_area_cvg@data$social_pressure <- study_area_cvg@data$social_pressure + study_area_cvg@data$protesta
  }
  study_area_cvg
}