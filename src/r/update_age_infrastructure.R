update_infrastructure_age <- function(study_area_cvg) {
  #update infrastructure related atributes
  #update_age_infrastructure
  study_area_cvg@data$antiguedad_D<-study_area_cvg@data$antiguedad_D+1
  study_area_cvg@data$antiguedad_Ab<-study_area_cvg@data$antiguedad_Ab+1

  #update_capacity of the system
  study_area_cvg@data$capac_w<-study_area_cvg@data$capac_w *(1- decay_infra)
  #update capacity index
  #FIDEL
  #The proportion of people without infrastructure increases proportionally to
  #the growthof the population in each delegation
  study_area_cvg@data$FALTA_IN=study_area_cvg@data$FALTA_IN * (1+(1-study_area_cvg@data$FALTA_IN)*study_area_cvg@data$pop_growth)
  study_area_cvg@data$falta_dren=study_area_cvg@data$falta_dren* (1+(1-study_area_cvg@data$falta_dren)*study_area_cvg@data$pop_growth)

  study_area_cvg
}