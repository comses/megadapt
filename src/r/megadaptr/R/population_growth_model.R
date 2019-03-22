#Calculate a new population per census block given the growth rate
#' Generate population data
#'
#' @param data.frame with variables population_size and population growth
#' @return data frame with new population size
calculate_new_population<-function(study_data){
  study_data$poblacion<-study_data$poblacion+study_data$tc_pob*study_data$poblacion
  #study_data$Pop_density=study_data$poblacion/study_data$area
  study_data
}

