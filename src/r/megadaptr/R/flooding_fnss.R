flooding_index_calculate <- function(weights, study_data) {
  w <- weights / sum(weights)

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
    #xmax = 2064.34,
    xmax = 237.81,
    xmin = 0,
    #gama = 0.197
    gama = 0.064
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

  w_historic_flooding_freq = w['flooding']
  w_f_prec_v = w['precipitation']
  w_fv_non_potable_capacity = w['capacity']
  w_f_esc = w['runoff']

  flooding_index = (w_historic_flooding_freq * fv_historic_flooding_freq) +
    (w_f_prec_v * fv_f_prec_v) +
    (w_fv_non_potable_capacity * fv_non_potable_capacity) +
    (w_f_esc * fv_f_esc)

  flooding_index
}

flooding_deserialize <- function(config) {
  config <- do.call(flooding_config_create, config)

  weights <- as.numeric(config$weights)
  names(weights) <- names(config$weights)

  index_weights <- as.numeric(config$index_weights)
  names(index_weights) <- names(config$index_weights)

  extreme_index = readr::read_csv(config$extreme_index_path)
  extreme_occurrence = readr::read_csv(config$extreme_occurrence_path)

  flooding_delta_method_fnss_create(
    weights = weights,
    index_weights = index_weights,
    extreme_index = extreme_index,
    extreme_occurrence = extreme_occurrence
    )
}

flooding_config_create <- function(
  weights = list(
    capacity = 0.5,
    precipitation = 1,
    runoff = 0.5),
  index_weights = list(
    capacity = 1,
    flooding = 1,
    precipitation = 1,
    runoff = 1),
  extreme_index_path = data_dir("extreme_events/bd_ageb_tr.csv"),
  extreme_occurrence_path = data_dir("extreme_events/extreme_ocurrence_0.csv")
  ) {



  list(
    weights = weights,
    index_weights = index_weights,
    extreme_index_path = extreme_index_path,
    extreme_occurrence_path = extreme_occurrence_path

  )
}

#' Create a flooding delta model
#'
#' @export
#' @param weights a vector of weights
#' @param index_weights a vector of weight for calcutating the flooding index
#' @return an object of weights of class 'flooding_delta_method_fnss'
flooding_delta_method_fnss_create <-
  function(weights = c(capacity = 0.5,
                       precipitation = 1,
                       runoff = 0.5),
           index_weights = c(
             capacity = 1,
             flooding = 1,
             precipitation = 1,
             runoff = 1
           ),
           extreme_index,
           extreme_occurrence) {
    index_weights <- index_weights / sum(index_weights)
    prepend_class(list(weights = weights, index_weights = index_weights, extreme_occurrence = extreme_occurrence, extreme_index = extreme_index), 'flooding_delta_method_fnss')
  }


#' Flooding index delta method
#'
#' @export
#' @method call_fnss flooding_delta_method_fnss
#' @inheritParams call_fnss
#' @param study_data census block cross section with
call_fnss.flooding_delta_method_fnss <-
  function(fnss, study_data, year, ...) {
    w <- fnss$weights
    index_weights <- fnss$index_weights
    cap_init <- study_data$sewer_system_capacity_max * 0.5
    precip_mean <- study_data$precipitation_volume_mean
    runoff_mean <- study_data$runoff_volume_mean

    change_capacity <- ifelse(cap_init > 0, (study_data$sewer_system_capacity - cap_init)/cap_init, 0)
    change_precipitation <- (study_data$precipitation_volume - precip_mean)/precip_mean
    change_runoff <- pmax(pmin((study_data$runoff_volume - runoff_mean)/runoff_mean,1),-1)
    #change_runoff <- (change_runoff - mean(change_runoff))/var(change_runoff)^0.5
    flooding_mean <- study_data$resident_reports_flooding_count_mean
    flooding_event_count <- flooding_mean * (1 -
      w['capacity']*change_capacity +
      w['precipitation']*change_precipitation +
      w['runoff']*change_runoff)






    #flooding_index = flooding_index_calculate(weights = index_weights, study_data = study_data)
    flooding_index <- sapply(
      flooding_event_count,
      FUN=logistica_invertida,
      k=0.059,
      center=5,
      xmin=0,
      xmax=16)



    sd <- study_data %>%
      dplyr::inner_join(fnss$extreme_index, by = PK_JOIN_EXPR)
    year_b <- year
    extreme_this_year <- fnss$extreme_occurrence %>% dplyr::filter(year == year_b)
    field <- paste0("tr", extreme_this_year$tr)

    flooding_index <- pmin(sd[[field]], flooding_index)
    #flooding_index <- (0.5 * sd[[field]]) + (0.5 * flooding_index)
    #flooding_index <- flooding_index^(sd[[field]])


    tibble::tibble(
      censusblock_id = study_data$censusblock_id,
      flooding_event_count = pmax(flooding_event_count, 0),
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
flooding_initialize <- function(flooding_fnss, study_data, year) {
  study_data %>%
    dplyr::inner_join(call_fnss(flooding_fnss, study_data = study_data, year = year), by = PK_JOIN_EXPR) %>%
    dplyr::mutate(
      initial_flooding_index=flooding_index
    )
}
