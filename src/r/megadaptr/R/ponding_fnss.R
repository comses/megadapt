ponding_config_call <- function(name, args) {
  checkmate::assert_subset(name, choices = c('ponding_index_fnss_create', 'ponding_delta_method_fnss_create'))
  weights <- numeric(args)
  names(weights) <- names(args)
  get(name)(weights)
}

#' A function to calculate a ponding index using value functions
#'
#' @export
#' @param weights A vector of weigth parameters of length=4.
#' @param study_data data.frame with variables "precipitation_volume", "runoff_volume", "resident_reports_ponding_count" and "sewer_system_capacity".
#' @return A data.frame with variables "censusblock_id" and "ponding_index". The ponding index will have values between 0 and 1.
ponding_index_calculate <- function(weights, study_data) {
  checkmate::assert(
    checkmate::check_numeric(weights),
    checkmate::check_names(names(weights), permutation.of = c('precipitation', 'runoff', 'ponding', 'capacity')),
    combine = 'and'
  )

  weights <- weights / sum(weights)

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

  fv_historic_ponding_freq <- sapply(
    study_data$resident_reports_ponding_count_mean,
    FUN = logistica_invertida,
    xmax = 10,
    xmin = 0,
    k = 0.108,
    center = 3.5
  )# min+(max-min)/2 ==49

  w_historic_ponding_freq = weights['ponding']
  w_f_prec_v = weights['precipitation']
  w_non_potable_capacity = weights['capacity']
  w_f_esc = weights['runoff']

  ponding_index = (w_historic_ponding_freq * fv_historic_ponding_freq) +
    (w_f_prec_v * fv_f_prec_v) +
    (w_non_potable_capacity * fv_non_potable_capacity) +
    (w_f_esc * fv_f_esc)

  ponding_index
}

ponding_deserialize = function(config) {
  config <- do.call(ponding_config_create, config)

  weights <- as.numeric(config$weights)
  names(weights) <- names(config$weights)

  index_weights <- as.numeric(config$index_weights)
  names(index_weights) <- names(config$index_weights)

  ponding_delta_method_fnss_create(
    weights = weights,
    index_weights = index_weights)
}

ponding_config_create <- function(
  weights = list(
    capacity = 0.5,
    precipitation = 1,
    runoff = 0.5
  ),
  index_weights = list(
    capacity = 1,
    ponding = 1,
    precipitation = 1,
    runoff = 1
  )) {
  list(
    weights = weights,
    index_weights = index_weights
  )
}

################################################

#' Create an object of class "ponding_delta_method_fnss".
#'
#' @export
#' @param weights A vector of weigth parameters of length=3
#' @param index_weights A vector weights for caclulating the ponding index
#' @return An object of class 'ponding_delta_method_fnss' to be used as arguments into the ponding index using "call_fnss.ponding_delta_method_fnss".
ponding_delta_method_fnss_create <- function(
  weights = c(
    capacity = 0.5,
    precipitation = 1,
    runoff = 0.5
  ),
  index_weights = c(
    capacity = 1,
    ponding = 1,
    precipitation = 1,
    runoff = 1
  )) {
  index_weights <- index_weights / sum(index_weights)
  checkmate::assert(
    checkmate::check_numeric(weights),
    checkmate::check_names(names(weights), permutation.of = c('capacity', 'precipitation', 'runoff')),
    combine = 'and'
  )

  prepend_class(list(weights = weights, index_weights = index_weights), 'ponding_delta_method_fnss')
}


#' A function to calculate a ponding index using relative change in runoff, precipitation, and sewer system capacity.
#'
#' @export
#' @method call_fnss ponding_delta_method_fnss
#'
#' @inheritParams call_fnss
#' @param fnss An object with weights parameters created with \code{\link{ponding_delta_method_fnss_create}}
#' @param study_data data.frame with variables "precipitation_volume", "runoff_volume", "resident_reports_ponding_count" and "sewer_system_capacity".
#' @return A data.frame with variables "censusblock_id" and "ponding_index". The ponding index will have values between 0 and 1.
call_fnss.ponding_delta_method_fnss <- function(fnss, study_data, ...) {
  w <- fnss$weights
  index_weights <- fnss$index_weights
  cap_init <- study_data$sewer_system_capacity_max * 0.5
  precip_mean <- study_data$precipitation_volume_mean
  runoff_mean <- study_data$runoff_volume_mean

  change_capacity <- ifelse(cap_init > 0, (study_data$sewer_system_capacity - cap_init)/cap_init, 0)
  change_precipitation <- (study_data$precipitation_volume - precip_mean)/precip_mean
  change_runoff <- ifelse(runoff_mean > 0, pmax(pmin((study_data$runoff_volume - runoff_mean)/runoff_mean,1),-1), 0)
  #change_runoff <- (change_runoff - mean(change_runoff))/var(change_runoff)^0.5
  ponding_mean <- study_data$resident_reports_ponding_count_mean
  ponding_event_count = ponding_mean * (1 -
    w['capacity']*change_capacity +
    w['precipitation']*change_precipitation +
    w['runoff']*change_runoff)
  #ponding_index = ponding_index_calculate(weights = index_weights, study_data = study_data)
  ponding_index <- sapply(
    ponding_event_count,
    FUN=logistica_invertida,
    k=0.13,
    center=3.1,
    xmin=0,
    xmax=13)

  tibble::tibble(
    censusblock_id = study_data$censusblock_id,
    ponding_event_count = pmax(ponding_event_count, 0),
    ponding_index = ponding_index
  )
}

#' A value function object of class "value function" to apply in specifically in the delta model component
#' @param ponding a ponding component.
#' @param study_data A data frame for the study area.
#' @return A value function object to include in the delta method ponding calculation.
value_function.ponding_delta_method_fnss <- function(ponding, study_data) {
  sapply(
    study_data$ponding_index,
    FUN=logistica_invertida,
    k=0.13,
    center=3.1,
    xmin=0,
    xmax=13)
}

#' The initializilation part of the flooding component
#' @param ponding_fnss A ponding component.
#' @param study_data A data frame with the spatial units and associated fields.
#' @return an updated study_data including the initial value of the flooding index.
ponding_initialize <- function(ponding_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(ponding_fnss, study_data = study_data), by = PK_JOIN_EXPR)
}
