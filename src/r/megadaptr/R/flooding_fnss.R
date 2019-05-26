flooding_index_fnss_create <- function(weights = c(
  capacity = 1,
  flooding = 1,
  precipitation = 1,
  runoff = 1
)) {
  weights <- weights / sum(weights)
  prepend_class(weights, 'flooding_index_fnss')
}

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
flooding_delta_method_fnss_create <-
  function(weights = c(capacity = 1,
                       precipitation = 1,
                       runoff = 1)) {
    weights <- weights / sum(weights)
    prepend_class(weights, 'flooding_delta_method_fnss')
  }

call_fnss.flooding_delta_method_fnss <-
  function(flooding_fnss, study_data) {
    w <- flooding_fnss
    cap_init <- study_data$sewer_system_capacity
    precip_mean <- study_data$precipitation_volume_mean
    runoff_mean <- study_data$runoff_volume_mean

    change_capacity <- (study_data$sewer_system_capacity - cap_init)/cap_init
    change_precipitation <- (study_data$precipitation_volume - precip_mean)/precip_mean
    change_runoff <- (study_data$runoff_volume - runoff_mean)/runoff_mean
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

value_function.flooding_delta_method_fnss <- function(flooding, study_data) {
  sapply(
    study_data$flooding_index,
    FUN=logistica_invertida,
    k=0.13,
    center=4,
    xmin=0,
    xmax=16)
}

flooding_initialize <- function(flooding_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(flooding_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}
