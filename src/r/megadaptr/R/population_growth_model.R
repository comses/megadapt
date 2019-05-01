#' Grow census block population according to its growth rate
#'
#' @param study_data data.frame with variables population_size and population growth
#' @return data frame with new population size
update_population_exponential <- function(study_data){
  study_data$poblacion <- study_data$poblacion*(1 + study_data$tc_pob)
  #study_data$Pop_density=study_data$poblacion/study_data$area
  study_data
}

