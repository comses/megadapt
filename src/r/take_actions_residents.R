take_actions_residents <- function(site_suitability) {
  distance_ideal_House_mod_lluvia <- site_suitability$distance_ideal_House_mod_lluvia
  distance_ideal_House_mod_agua <- site_suitability$distance_ideal_House_mod_agua
  distance_ideal_protest <- site_suitability$distance_ideal_protest

  # residents decisions
  # find agebs that will adapt to reduce effects of flooding
  HM_LL <- which(distance_ideal_House_mod_lluvia > distance_ideal_House_mod_agua)
  # find agebs that will adapt to reduce effects of water scarcity
  HM_Agua <- which(distance_ideal_House_mod_lluvia < distance_ideal_House_mod_agua)

  # From all census blocks that will adapt to reduce the effect of water scarcity
  # find those that will protest
  agebs_que_protestan <- HM_Agua[which(distance_ideal_protest[HM_Agua] > distance_ideal_House_mod_agua[HM_Agua])]

  list(agebs_que_protestan = agebs_que_protestan, HM_LL = HM_LL, HM_Agua = HM_Agua)
}
