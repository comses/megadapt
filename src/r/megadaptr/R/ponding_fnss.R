#' Create an object of class "ponding_index_fnss".
#'
#'@param weights A vector of weigth parameters of length=4.
#'@return An object of class 'ponding_index_fnss' to be used as arguments into the ponding index with value functions.

ponding_index_fnss_create <-
  #' @export
  function(weights = c(
             precipitation = 0.25,
             runoff = 0.25,
             ponding = 0.25,
             capacity = 0.25
           )) {

  weights <- weights / sum(weights)
  prepend_class(weights, 'ponding_index_fnss')
  }

#' A function to calculate a ponding index using value functions
#'
#'@param ponding_index_fnss An object with weights parameters created with \code{\link{ponding_index_fnss_create}}
#'@param study_data data.frame with variables "precipitation_volume", "runoff_volume", "resident_reports_ponding_count" and "sewer_system_capacity".
#'@return A data.frame with variables "censusblock_id" and "ponding_index". The ponding index will have values between 0 and 1.
call_fnss.ponding_index_fnss <- function(ponding_index_fnss, study_data) {
  #' @export
  #' @method call_fnss ponding_index_fnss
  fv_f_prec_v <- sapply(
    study_data$precipitation_volume,
    FUN = convexa_decreciente,
    xmax =  8930363.15853,
    # [mm/km2]==  1202 mm/year
    xmin = 10590.85,
    # [mm/km2]
    gama = 0.035
  )

  fv_non_potable_capacity <- sapply(
    study_data$sewer_system_capacity,
    FUN = convexa_creciente,
    xmax = 2064.34,
    xmin = 0,
    gama = 0.197
  )


  fv_f_esc <- sapply(
    study_data$runoff_volume,
    FUN = convexa_decreciente,
    xmax = 504,
    xmin = 0,
    gama = 0.035
  )

  fv_historic_ponding_freq <- sapply(
    study_data$resident_reports_ponding_count,
    FUN = logistica_invertida,
    xmax = 10,
    xmin = 0,
    k = 0.108,
    center = 3.5
  )# min+(max-min)/2 ==49


  weights <- ponding_index_fnss
  w_historic_ponding_freq = weights['ponding']
  w_f_prec_v = weights['precipitation']
  w_non_potable_capacity = weights['capacity']
  w_f_esc = weights['runoff']

  ponding_index = 1 - (w_historic_ponding_freq * fv_historic_ponding_freq) +
    (w_f_prec_v * fv_f_prec_v) +
    (w_non_potable_capacity * fv_non_potable_capacity) +
    (w_f_esc * fv_f_esc)

  tibble::tibble(censusblock_id = study_data$censusblock_id,
                 ponding_index = ponding_index) #crear variable en dataframe
}

################################################

#' Create an object of class "ponding_delta_method_fnss".
#'
#'@param weights A vector of weigth parameters of length=3
#'@return An object of class 'ponding_delta_method_fnss' to be used as arguments into the ponding index using "call_fnss.ponding_delta_method_fnss".
ponding_delta_method_fnss_create <- function(
  #' @export
  weights = c(
    capacity = 1,
    precipitation = 1,
    runoff = 1
  )) {
  weights <- weights / sum(weights)
  prepend_class(weights, 'ponding_delta_method_fnss')
}


#' A function to calculate a ponding index using relative change in runoff, precipitation, and sewer system capacity.
#'
#'@param ponding_delta_method_fnss An object with weights parameters created with \code{\link{ponding_delta_method_fnss_create}}
#'@param study_data data.frame with variables "precipitation_volume", "runoff_volume", "resident_reports_ponding_count" and "sewer_system_capacity".
#'@return A data.frame with variables "censusblock_id" and "ponding_index". The ponding index will have values between 0 and 1.
call_fnss.ponding_delta_method_fnss <- function(ponding_delta_method_fnss, study_data) {
  #' @export
  #' @method call_fnss ponding_delta_method_fnss
  w <- ponding_delta_method_fnss
  cap_init <- study_data$sewer_system_capacity_initial
  precip_mean <- study_data$precipitation_volume_mean
  runoff_mean <- study_data$runoff_volume_mean

  change_capacity <- (study_data$sewer_system_capacity - cap_init)/cap_init
  change_precipitation <- (study_data$precipitation_volume - precip_mean)/precip_mean
  change_runoff <- (study_data$runoff_volume - runoff_mean)/runoff_mean
  ponding_mean <- study_data$resident_reports_ponding_count_mean
  ponding_index = ponding_mean -
    w['capacity']*change_capacity +
    w['precipitation']*change_precipitation +
    w['runoff']*change_runoff
  tibble::tibble(
    censusblock_id = study_data$censusblock_id,
    ponding_index = ponding_index
  )
}

ponding_initialize <- function(ponding_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(ponding_fnss, study_data = study_data), by = PK_JOIN)
}
