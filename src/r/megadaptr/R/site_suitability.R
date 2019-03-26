#' Determine suitability of spatial units
#' @param study_data Data frame with spatial units (e.g. shapefile)
#' @param value_function_config A set of value functions and their parameters
#'   \describe{
#'   \item{antiguedad_dist}{FPotable water infrastructure age in years}
#'   \item{critic_z}{Indicator variable of whether census block has less than 4 hours of water per day}
#'   \item{falta_dist}{Proportion of houses in census block without water}
#'   }
#' @param week_of_year The number of weeks up until the present week of the current year
#' @return data frame with pk and cumulative number of days without clean water this week and year by census block

determine_site_suitability <- function(study_data, value_function_config, mental_models) {

  sewer_age <- value_function_config$sewer_age
  shortage_age <- value_function_config$shortage_age
  shortage_failures <- value_function_config$shortage_failures
  hydraulic_pressure_failure <- value_function_config$hydraulic_pressure_failure
  subsidence <- value_function_config$subsidence

  # site_suitability
  # 1)update value functions sacmex
  # a) age infrastructure drainage
  vf_A_D <- sapply(study_data$antiguedad_dren,
                   FUN = logistic_invertida,
                   center = sewer_age$center,
                   k = sewer_age$k,
                   xmax = sewer_age$max,
                   xmin = sewer_age$min)

  # b) age infrastructure Abastecimiento
  vf_A_Ab <- sapply(study_data$antiguedad_dist,
                    FUN = campana_invertida,
                    center = shortage_age$center,
                    a = shortage_age$a,
                    xmax = shortage_age$max,
                    xmin = shortage_age$min)


  # c)Drainage capacity
  vf_Cap_D<-sapply(study_data$q100,FUN = capacity_drainage_vf,sat=1,x_max=1500,x_min=0)

  # d)falta
  vf_falta_dist <- sapply(100 * study_data$falta_dist, FUN = lack_of_infrastructure_vf)
  vf_falta_dren <- sapply(100 * study_data$falta_dren, FUN = lack_of_infrastructure_vf)

  # c)potable water system capacity
  vf_Cap_Ab <- rep(1, length(study_data$falta_dist))

  # d) falla Ab
  vf_falla_dist <- 1 - sapply(study_data$falla_dist,
                         FUN = convexa_creciente,
                         gama = shortage_failures$gama,
                         xmax = shortage_failures$max,
                         xmin = shortage_failures$min)

  # falla D
  vf_fall_dren <- rep(1, length(study_data$falla_dren))


  # e)water scarcity
  #vf_scarcity_sacmex <- sapply(study_data$days_wn_water_year, FUN = scarcity_sacmex_vf) # scarcity_annual is calculated dynamically
  # flooding #cchange to flooding
  vf_flood <- sapply(study_data$inunda, FUN = ponding_vf)
  # Ponding
  vf_pond <- sapply(study_data$encharca, FUN = ponding_vf)
  # social_pressure
  vf_SP <-study_data$fv_reportes*(1-study_data$scarcity_index)
  #  vf_SP <- sapply(study_data$social_pressure, FUN = social_pressure_vf)

  # rainfall
  vf_rain <- sapply(study_data$f_prec_v, FUN = rainfall_vf) #change value function range
  # run-off/escurrimiento
  vf_run_off <- sapply(study_data$f_esc, FUN = run_off_vf)

  # garbage
  vf_garbage <- sapply(study_data$basura,
                       FUN = drainages_clogged_vf,
                       amplitude = 500000,
                       Valor_minimo_Y_en_X = max(study_data$basura))

  # subsidance
  vf_subside <- sapply(study_data$subsidenci,
                       FUN = logistic_invertida,
                       k = subsidence$k,
                       xmin = subsidence$min,
                       xmax = subsidence$max,
                       center = subsidence$center)

  # hydraulic pressure
  vf_hid_pressure <- sapply(study_data$pres_hid,
                            FUN = logistic_vf,
                            k = hydraulic_pressure_failure$k,
                            center = hydraulic_pressure_failure$center,
                            xmax = hydraulic_pressure_failure$max,
                            xmin = hydraulic_pressure_failure$min)

  # monto ##!!!#no information about this variable
  vf_monto <- rep(1, length(study_data$ageb_id))
  # hidraulic cost
  vf_GH <- sapply(study_data$q100, FUN = Value_Function_cut_offs, xmax = max(study_data$gasto), xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2))
  # water supply
  vf_Abaste <- sapply(study_data$abastecimi, FUN = Value_Function_cut_offs, xmax = max(study_data$abastecimi, na.rm = T))
  # Petitions from delegations
  vf_pet_del_dr <- sapply(study_data$pet_del_dr, FUN = Peticion_Delegaciones_vf)
  # petitions from users
  #vf_pet_us_d <- sapply(study_data$pet_usr_d, FUN = Peticiones_usuarios_vf, xmax = max(study_data$pet_usr_d, na.rm = T))
  vf_pet_us_d <- rep(1, length(study_data$ageb_id))
  # Media pressure
  vf_pres_medios <- sapply(study_data$pres_med, FUN = pression_medios_vf)


  # 2)update value functions residents

  # Urban growth
  vf_UG <- sapply(study_data$crec_urb, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$crec_urb, na.rm = T))

  # Water quality
  vf_WQ <- sapply(study_data$cal_agua, FUN = water_quality_residents_vf)

  # Health
  vf_H <- sapply(study_data$enf_14, FUN = health_vf)

  # water scarcity residents
  #vf_scarcity_residents <- sapply(study_data$days_wn_water_two_weeks, FUN = scarcity_residents_empirical_vf, tau = 12) # days_wn_water need to be define

  # ponding residents
  vf_pond <- sapply(study_data$encharca, FUN = ponding_vf)

  # Deviation of potable water
  vf_DA <- sapply(study_data$desv_agua, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$desv_agua, na.rm = T))

  # Waste of water
  vf_Desp_A <- sapply(study_data$desp_agua, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$desp_agua, na.rm = T))

  # Insufiency in water
  vf_Agua_insu <- sapply(study_data$days_wn_water_two_weeks, FUN = scarcity_residents_vf) # days_wn_water need to be define

  # Lack of drainage system
  fv_falta_dren <- sapply(100 * (1 - study_data$falta_dren), FUN = lack_of_infrastructure_vf)

  # Population growth
  fv_crecimiento_pop <- sapply(study_data$pop_growth, FUN = urban_growth_f, xmax = max(study_data$pop_growth, na.rm = T))
  fv_crecimiento_pop[which(fv_crecimiento_pop<0)]<-1
  # Lickages
  #fv_fugas <- sapply(study_data$fugas, FUN = Value_Function_cut_offs, xcuts = c(0.5, 0.75, 0.875, 0.937), ycuts = c(1, 0.8, 0.6, 0.4, 0.2), xmax = max(study_data$FUGAS, na.rm = T))
  fv_fugas <- sapply(study_data$fugas,
                     FUN = gaussian,
                     a = 10,
                     center = 0,
                     xmax = min(study_data$fugas),
                     xmin = max(study_data$fugas))
  ################################################################################################################
  # join all converted attributes into a single matrix
  all_C_ab <- cbind(
    vf_A_Ab,
    vf_Cap_Ab,
    vf_fall_dist,
    vf_falta_dist,
    vf_monto,
    vf_hid_pressure,
    vf_WQ,
    scarcity_index,#vf_scarcity_sacmex,
    vf_pond,
    vf_Abaste,
    vf_pet_del_dr,
    vf_pres_medios,
    vf_SP
  )


  all_C_D <- cbind(
    vf_garbage,
    vf_run_off,
    vf_subside,
    vf_rain,
    vf_A_D,
    vf_Cap_D,
    vf_fall_dren,
    vf_falta_D,
    vf_pet_del_dr,
    vf_pet_us_d,
    vf_pres_medios,
    vf_pond,
    vf_flood
  )
  #########################################################################
  # house modification water supply
  C_R_HM <- cbind(
    vf_WQ,
    vf_UG,
    vf_Desp_A,
    vf_fall_dist,    #Eficacia.del.servicio
    vf_falta_dist,
    rep(1, length(vf_falta_dist)),
    vf_Agua_insu,
    rep(1, length(vf_falta_dist)), # flooding do not influence protests or water capture
    vf_H
  )
  # protest
  C_R_protest <- cbind(
    vf_WQ,
    vf_UG,
    vf_Desp_A,
    vf_fall_dist,
    fv_falta_dist,
    rep(1, length(fv_falta_dist)),
    scarcity_index,#vf_scarcity_residents,
    rep(1, length(fv_falta_dist)), # flooding do not influence protests or water capture
    vf_H
  )

  # house modification flooding
  C_R_D <- cbind(
    vf_WQ,
    vf_UG,
    rep(1, length(fv_falta_dren)),
    rep(1, length(fv_falta_dren)),
    fv_falta_dren,
    vf_garbage,
    rep(1, length(fv_falta_dren)), # scarcity does not affect floding
    vf_flood,
    vf_H
  )


  ################################################################################################################
  # 2)calculate distance for each census block for action mantainance and build new infrastructure
  sacmcx_criteria_d <- as.vector(mental_models$sacmcx$criteria$d)
  sacmcx_alternative_weights_d <- mental_models$sacmcx$alternative_weights$d
  distance_ideal_A1_D <- sweep(as.matrix(all_C_D),
                               MARGIN = 2,
                               sacmcx_criteria_d / sum(sacmcx_criteria_d),
                               FUN = ideal_distance,
                               z = sacmcx_alternative_weights_d[1] / sum(sacmcx_alternative_weights_d)) # "Mantenimiento"
  distance_ideal_A2_D <- sweep(as.matrix(all_C_D),
                               MARGIN = 2,
                               sacmcx_criteria_d / sum(sacmcx_criteria_d),
                               FUN = ideal_distance,
                               z = sacmcx_alternative_weights_d[2] / sum(sacmcx_alternative_weights_d)) # "Nueva_infraestructura"

  sacmcx_criteria_ab <- as.vector(mental_models$sacmcx$criteria$ab)
  sacmcx_alternative_weights_s <- mental_models$sacmcx$alternative_weights$s
  distance_ideal_A1_Ab <- sweep(as.matrix(all_C_ab),
                                MARGIN = 2,
                                sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
                                FUN = ideal_distance,
                                z = sacmcx_alternative_weights_s[4] / sum(sacmcx_alternative_weights_s[c(4, 5)])) # "Mantenimiento"
  distance_ideal_A2_Ab <- sweep(as.matrix(all_C_ab),
                                MARGIN = 2,
                                sacmcx_criteria_ab / sum(sacmcx_criteria_ab),
                                FUN = ideal_distance,
                                z = sacmcx_alternative_weights_s[5] / sum(sacmcx_alternative_weights_s[c(4, 5)])) # "Nueva_infraestructura"

  # Residents
  # distance_ideal_protest<-sweep(as.matrix(C_R_protest[,-c(6,8)]),MARGIN=2,as.vector(Criteria_residents_Iz[-c(6,8)])/sum(as.vector(Criteria_residents_Iz[-c(6,8)])),FUN=ideal_distance,z=alternative_weights_Iz[5]/sum(alternative_weights_Iz[c(4,5)]))# "Protests"
  alternative_weights_iz <- mental_models$residents$alternative_weights$iz
  criteria_iz <- as.vector(mental_models$residents$criteria$iz)

  distance_ideal_protest <- 1 - vf_scarcity_residents
  distance_ideal_House_mod_lluvia <- sweep(as.matrix(C_R_D[, -c(3, 4, 7)]),
                                           MARGIN = 2,
                                           criteria_iz[-c(3, 4, 7)] / sum(criteria_iz[-c(3, 4, 7)]),
                                           FUN = ideal_distance,
                                           z = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])) # "House modification"
  distance_ideal_House_mod_agua <- sweep(as.matrix(C_R_HM[, -c(6, 8)]),
                                         MARGIN = 2,
                                         criteria_iz[-c(6, 8)] / sum(criteria_iz[-c(6, 8)]),
                                         FUN = ideal_distance,
                                         z = alternative_weights_iz[4] / sum(alternative_weights_iz[c(4, 5)])) # "House modification"

  ################################################################################################################
  # 3) save value function and distance matrix as a shape file
  # Output_value_function <- study_area_cvg
  # Output_value_function@data <- cbind(Output_value_function@data, all_C_D, all_C_ab, distance_ideal_A1_D, distance_ideal_A2_D, distance_ideal_A1_Ab, distance_ideal_A2_Ab, distance_ideal_House_mod_lluvia, distance_ideal_House_mod_lluvia, distance_ideal_House_mod_agua)

  list(
    distance_ideal_A1_D = distance_ideal_A1_D,
    distance_ideal_A2_D = distance_ideal_A2_D,
    distance_ideal_A1_Ab = distance_ideal_A1_Ab,
    distance_ideal_A2_Ab = distance_ideal_A2_Ab,
    distance_ideal_protest = distance_ideal_protest,
    distance_ideal_House_mod_lluvia = distance_ideal_House_mod_lluvia,
    distance_ideal_House_mod_agua = distance_ideal_House_mod_agua
    # Output_value_function = Output_value_function
  )
}
