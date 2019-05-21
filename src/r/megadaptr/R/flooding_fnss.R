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
      study_data$f_prec_v,
      FUN = convexa_decreciente,
      xmax =  8930363.15853,
      # [mm/km2]==  1202 mm/year
      xmin = 10590.85,
      # [mm/km2]
      gama = 0.035
    )

    fv_non_potable_capacity <- sapply(
      study_data$non_potable_capacity,
      FUN = convexa_creciente,
      xmax = 2064.34,
      xmin = 0,
      gama = 0.01975
    )


    fv_f_esc <- sapply(
      study_data$f_esc,
      FUN = convexa_decreciente,
      xmax = 504,
      xmin = 0,
      gama = 0.035
    )

    fv_historic_flooding_freq <- sapply(
      study_data$inunda,
      FUN = logistic_invertida,
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

    tibble::tibble(ageb_id = study_data$ageb_id,
                   flooding_index = flooding_index) #crear variable en dataframe
  }

flooding_excess_fnss_create <-
  function(weights = c(capacity = 1,
                       precipitation = 1,
                       runoff = 1)) {
    weights <- weights / sum(weights)
    prepend_class(weights, 'flooding_excess_fnss')
  }

call_fnss.flooding_excess_fnss <-
  function(flooding_fnss, study_data) {

  }
