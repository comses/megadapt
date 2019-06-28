
#' Create a flooding index model
#'
#' @export
#' @param weights weights to apply to capacity, flooding, precipitation and runoff value functions
#' @return a ponding index tibble
flooding_index_fnss_create <- function(weights = c(
  capacity = 1,
  flooding = 1,
  precipitation = 1,
  runoff = 1
)) {
  weights <- weights / sum(weights)
  prepend_class(weights, 'flooding_index_fnss')
}

#' Flooding index value function method
#'
#' @export
#' @method call_fnss flooding_index_fnss
#' @inheritParams call_fnss
call_fnss.flooding_index_fnss <-
  function(flooding_index_fnss, study_data) {
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
      gama = 0.01975
    )


    fv_f_esc <- sapply(
      study_data$runoff_volume,
      FUN = convexa_decreciente,
      xmax = 504,
      xmin = 0,
      gama = 0.035
    )

    fv_historic_flooding_freq <- sapply(
      study_data$resident_reports_flooding_count_mean,
      FUN = logistica_invertida,
      xmax = 8.0266,
      xmin = 0,
      k = 0.083,
      center = 4.013
    )# min+(max-min)/2 ==49


    #calculate weights for each factor
    #For now, weights are equal for all the factors: 1/3 for areas without runoff and
    #1/4 for areas with runoff

    w <- flooding_index_fnss
    w_historic_flooding_freq = w['flooding']
    w_f_prec_v = w['precipitation']
    w_fv_non_potable_capacity = w['capacity']
    w_f_esc = w['runoff']

    flooding_index = (w_historic_flooding_freq * fv_historic_flooding_freq) +
      (w_f_prec_v * fv_f_prec_v) +
      (w_fv_non_potable_capacity * fv_non_potable_capacity) +
      (w_f_esc * fv_f_esc)

    tibble::tibble(censusblock_id = study_data$censusblock_id,
                   flooding_index = flooding_index) #crear variable en dataframe
  }

value_function.flooding_index_fnss <- function(flooding, study_data) {
  study_data$flooding_index
}

#' Create a flooding delta model
#'
#' @export
#' @param weights a vector of weights
#' @return an object of weights of class 'flooding_delta_method_fnss'
flooding_delta_method_fnss_create <-
  function(weights = c(capacity = 1,
                       precipitation = 1,
                       runoff = 1)) {
    weights <- weights / sum(weights)
    prepend_class(weights, 'flooding_delta_method_fnss')
  }


#' Flooding index delta method
#'
#' @export
#' @method call_fnss flooding_delta_method_fnss
#' @inheritParams call_fnss
call_fnss.flooding_delta_method_fnss <-
  function(flooding_fnss, study_data) {
    w <- flooding_fnss
    cap_init <- study_data$sewer_system_capacity_max * 0.5
    precip_mean <- study_data$precipitation_volume_mean
    runoff_mean <- study_data$runoff_volume_mean

    change_capacity <- ifelse(cap_init > 0, (study_data$sewer_system_capacity - cap_init)/cap_init, 0)
    change_precipitation <- (study_data$precipitation_volume - precip_mean)/precip_mean
    change_runoff <- (study_data$runoff_volume - runoff_mean)
    change_runoff <- (change_runoff - mean(change_runoff))/var(change_runoff)^0.5
    flooding_mean <- study_data$resident_reports_flooding_count_mean
    flooding_index <- flooding_mean -
      w['capacity']*change_capacity +
      w['precipitation']*change_precipitation +
      w['runoff']*change_runoff

    tibble::tibble(
      censusblock_id = study_data$censusblock_id,
      flooding_index = flooding_index
    )
  }

#' A value function object of class "value function" to apply in specifically in the flooding component uisng the delta method
#' @param flooding a flooding component.
#' @param study_data A data frame for the study area.
#' @return A value function object to include in the delta method flooding calculation.
value_function.flooding_delta_method_fnss <- function(flooding, study_data) {
  sapply(
    study_data$flooding_index,
    FUN=logistica_invertida,
    k=0.13,
    center=4,
    xmin=0,
    xmax=16)
}

#' The initializilation part of the flooding component
#' @param flooding_fnss A flooding component.
#' @param study_data A data frame with the spatial units and associated fields.
#' @return an updated study_data including the initial value of the flooding index.
flooding_initialize <- function(flooding_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(flooding_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}
