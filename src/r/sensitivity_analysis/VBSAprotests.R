#Protests

update_protests <- function(study_area_cvg, resident_actions) {
  agebs_que_protestan <- resident_actions$agebs_que_protestan
  #If the decision is to protest  (in "take_actions_residents.R"),
  #Here the protest is triggered and saved.
  
  study_area_cvg@data$protesta[agebs_que_protestan] <- 1
  
  study_area_cvg
}