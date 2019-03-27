create_mental_models <- function(mm_water_operator_d_lim,
                                 mm_water_operator_s_lim,
                                 mm_iz) {
  # name criteria
  names_criteria_sacmex_s <-
    colnames(mm_water_operator_s_lim)[-c(1:5)]
  names_criteria_sacmex_d <-
    colnames(mm_water_operator_d_lim)[-c(1, 2)]

  # criteria values
  criteria_sacmcx_ab <- mm_water_operator_s_lim$Antiguedad[-c(1:5)]
  criteria_sacmcx_d <- mm_water_operator_d_lim$Antiguedad[-c(1:2)]

  # alternative names
  names_alternative_sacmex_s <-
    colnames(mm_water_operator_s_lim)[c(1:5)]
  names_alternative_sacmex_d <-
    colnames(mm_water_operator_d_lim)[c(1, 2)]

  alternative_weights_s <-
    mm_water_operator_s_lim$Antiguedad[c(1:5)]
  alternative_weights_d <-
    mm_water_operator_d_lim$Antiguedad[c(1:2)]

  names_criteria_resident_iz <- colnames(mm_iz)[-c(1:5)]
  names_alternative_Resident_iz <- colnames(mm_iz)[c(1:5)]

  criteria_residents_iz <- mm_iz$Compra.de.agua[-c(1:5)]
  alternative_weights_iz <- mm_iz$Compra.de.agua[c(1:5)]

  list(
    sacmcx = list(
      names_criteria=list(ab=names_criteria_sacmex_s,
                 d=names_criteria_sacmex_d),
      criteria = list(ab = criteria_sacmcx_ab,
                      d = criteria_sacmcx_d),
      names_alternatives=list(ab=names_alternative_sacmex_s,
                                d=names_alternative_sacmex_d),
      alternative_weights = list(d = alternative_weights_d,
                                 s = alternative_weights_s)
    ),
    residents = list(
      names_criteria=list(names_criteria_resident_iz),
      criteria = list(iz = criteria_residents_iz),
      names_alternatives=list(names_alternative_Resident_iz),
      alternative_weights = list(iz = alternative_weights_iz)
    )
  )
}
