determine_site_suitability <- function(study_area_cvg, fv_antiguedad_drenaje, logistic_invertida) {
  #site_suitability
  #1)update value functions sacmex
  #a) age infrastructure drainage
  cent= as.numeric(as.character(fv_antiguedad_drenaje$V2[2]))
  xmin_v= as.numeric(as.character(fv_antiguedad_drenaje$V2[3]))
  xmax_v=as.numeric(as.character(fv_antiguedad_drenaje$V2[4]))
  k_v=as.numeric(as.character(fv_antiguedad_drenaje$V2[5]))
  vf_A_D<-sapply(study_area_cvg@data$antiguedad_D,FUN = logistic_invertida,center=cent,k=k_v,xmax=xmax_v,xmin=xmin_v)
  
  #b) age infrastructure Abastecimiento
  a= as.numeric(as.character(fv_antiguedad_escasez$V2[1]))
  cent= as.numeric(as.character(fv_antiguedad_escasez$V2[3]))
  xmin_v= as.numeric(as.character(fv_antiguedad_escasez$V2[4]))
  xmax_v=as.numeric(as.character(fv_antiguedad_escasez$V2[5]))
  
  vf_A_Ab<-sapply(study_area_cvg@data$antiguedad_Ab,FUN = campana_invertida,center=cent,a=a,xmax=xmax_v,xmin=xmin_v)
  
  
  #c)Drainage capacity
  vf_Cap_D<-sapply(study_area_cvg@data$q100,FUN = capacity_drainage_vf,sat=1,x_max=200,x_min=0)
  
  #d)falta
  vf_falta_Ab<-sapply(100*study_area_cvg@data$V_SAGUA,FUN=lack_of_infrastructure_vf)
  vf_falta_D<-sapply(100*study_area_cvg@data$falta_dren,FUN=lack_of_infrastructure_vf)
  
  #c)potable water system capacity
  vf_Cap_Ab<-rep(1,length(study_area_cvg@data$V_SAGUA))
  
  #d) falla Ab
  gamma_v= as.numeric(as.character(fv_falla_escasez$V2[4]))
  xmax_v= as.numeric(as.character(fv_falla_escasez$V2[3]))
  xmin_v =as.numeric(as.character(fv_falla_escasez$V2[2]))
  vf_falla<- 1-sapply(study_area_cvg@data$falla_in,FUN=convexa_creciente, gama=gamma_v, xmax=xmax_v, xmin=xmin_v)
  
  #falla D
  vf_fall_D<-rep(1,length(study_area_cvg@data$falla_in))
  
  
  #e)water scarcity
  vf_scarcity_sacmex<-sapply(study_area_cvg@data$days_wn_water_year,FUN=scarcity_sacmex_vf)#scarcity_annual is calculated dynamically
  #flooding #cchange to flooding
  vf_flood<-sapply(study_area_cvg@data$encharca,FUN=ponding_vf)
  #Ponding
  vf_pond<-sapply(study_area_cvg@data$encharca,FUN=ponding_vf)
  #social_pressure
  vf_SP <-sapply(study_area_cvg@data$social_pressure,FUN=social_pressure_vf)
  
  #rainfall
  vf_rain<-sapply(study_area_cvg@data$PR_2008,FUN=rainfall_vf)
  #run-off/escurrimiento
  vf_run_off<-sapply(study_area_cvg@data$escurri,FUN=run_off_vf)
  
  #garbage
  vf_garbage<-sapply(study_area_cvg@data$BASURA/10000,FUN=drainages_clogged_vf)
  
  #subsidance
  center_v=as.numeric(as.character(fv_subsidencia$V2[2]))
  xmin_v=as.numeric(as.character(fv_subsidencia$V2[3]))
  xmax_v=as.numeric(as.character(fv_subsidencia$V2[4]))
  k_v=as.numeric(as.character(fv_subsidencia$V2[5]))
  vf_subside<-sapply(study_area_cvg@data$subsidenci,FUN=logistic_invertida,k=k_v,xmin=xmin_v,xmax=xmax_v,center=center_v)
  
  #hydraulic pressure
  cen<-as.numeric(as.character(fv_presion_hidraulica_escasez$V2[2]))
  min_v<-as.numeric(as.character(fv_presion_hidraulica_escasez$V2[3]))
  max_v<-as.numeric(as.character(fv_presion_hidraulica_escasez$V2[4]))
  k_v<-as.numeric(as.character(fv_presion_hidraulica_escasez$V2[5]))
  
  vf_hid_pressure<-sapply(study_area_cvg@data$pres_hid,FUN=logistic_vf,k=k_v,center=cen,xmax=xmax_v,xmin=xmin_v)
  
  #monto ##!!!#no information about this variable
  vf_monto<-rep(1,length(study_area_cvg@data$AGEB_ID))
  #gasto hidraulico
  vf_GH<-sapply(study_area_cvg@data$gasto,FUN=Value_Function_cut_offs,xmax=max(study_area_cvg@data$gasto),xcuts=c(0.5, 0.75, 0.875, 0.937),ycuts=c(1, 0.8, 0.6, 0.4, 0.2))
  #abastecimiento
  vf_Abaste<-sapply(study_area_cvg@data$abastecimi,FUN=Value_Function_cut_offs,xmax=max(study_area_cvg@data$abastecimi,na.rm=T))
  #peticiones de delegaciones
  vf_pet_del_dr<-sapply(study_area_cvg@data$pet_del_dr,FUN=Peticion_Delegaciones_vf)
  #peticiones de usuarions delegacionales
  vf_pet_us_d<-sapply(study_area_cvg@data$pet_usr_d,FUN=Peticiones_usuarios_vf,xmax=max(study_area_cvg@data$pet_usr_d,na.rm = T))
  
  #presion de medios
  vf_pres_medios<- sapply(study_area_cvg@data$PRES_MED,FUN=pression_medios_vf)
  
  
  #2)update value functions residents
  
  #Crecimiento urbano
  vf_UG<-sapply(study_area_cvg@data$crec_urb,FUN=Value_Function_cut_offs,xcuts=c(0.5, 0.75, 0.875, 0.937),ycuts=c(1, 0.8, 0.6, 0.4, 0.2),xmax=max(study_area_cvg@data$crec_urb,na.rm=T))
  
  #Water quality 
  vf_WQ<-sapply(study_area_cvg@data$cal_agua,FUN=water_quality_residents_vf)
  
  #salud
  vf_H<-sapply(study_area_cvg@data$ENF_14,FUN=health_vf)
  
  #water scarcity residents
  vf_scarcity_residents<-sapply(study_area_cvg@data$NOWater_twoweeks,FUN=scarcity_residents_empirical_vf,tau=12) #days_wn_water need to be define
  
  #ponding residents
  vf_pond<-sapply(study_area_cvg@data$encharca,FUN=ponding_vf)
  
  #"Desviacion de agua" 
  vf_DA<-sapply(study_area_cvg@data$desv_agua,FUN=Value_Function_cut_offs,xcuts=c(0.5, 0.75, 0.875, 0.937),ycuts=c(1, 0.8, 0.6, 0.4, 0.2),xmax=max(study_area_cvg@data$desv_agua,na.rm=T))
  
  #"Desperdicio de agua" 
  vf_Desp_A<-sapply(study_area_cvg@data$desp_agua,FUN=Value_Function_cut_offs,xcuts=c(0.5, 0.75, 0.875, 0.937),ycuts=c(1, 0.8, 0.6, 0.4, 0.2),xmax=max(study_area_cvg@data$desp_agua,na.rm=T))
  
  #agua insuficiente
  vf_Agua_insu<-sapply(study_area_cvg@data$days_wn_water_month,FUN=scarcity_residents_vf) #days_wn_water need to be define
  
  #falta infrastructura drenaje
  fv_falta<-sapply(100*(1 - study_area_cvg@data$falta_dren),FUN=lack_of_infrastructure_vf)
  
  #crecimiento poblacional
  fv_crecimiento_pop<-sapply(study_area_cvg@data$pop_growth,FUN=urban_growth_f,xmax=max(study_area_cvg@data$pop_growth,na.rm=T))
  
  #fugas
  fv_fugas<-sapply(study_area_cvg@data$FUGAS, FUN=Value_Function_cut_offs,xcuts=c(0.5, 0.75, 0.875, 0.937),ycuts=c(1, 0.8, 0.6, 0.4, 0.2),xmax=max(study_area_cvg@data$FUGAS,na.rm=T))
  
  ################################################################################################################
  #join all converted attributes into a single matrix
  all_C_ab<-cbind(vf_A_Ab,
                  vf_Cap_Ab,
                  vf_falla,
                  vf_falta_Ab,
                  vf_monto,
                  vf_hid_pressure,
                  vf_WQ,
                  vf_scarcity_sacmex,
                  vf_pond,
                  vf_Abaste,
                  vf_pet_del_dr,
                  vf_pres_medios,
                  vf_SP)
  
  
  all_C_D<-cbind(vf_garbage,
                 vf_run_off,
                 vf_subside,
                 vf_rain,
                 vf_A_D,
                 vf_Cap_D,
                 vf_fall_D,
                 vf_falta_D,
                 vf_pet_del_dr,
                 vf_pet_us_d,
                 vf_pres_medios,
                 vf_pond,
                 vf_flood
  )     
  #########################################################################
  #house modification water supply 
  C_R_HM<-cbind(vf_WQ,
                vf_UG,
                vf_Desp_A,
                fv_fugas,
                fv_falta,
                rep(1,length(fv_falta)),
                vf_Agua_insu,
                rep(1,length(fv_falta)), #flooding do not influence protests or water capture
                vf_H)
  #protest
  C_R_protest<-cbind(vf_WQ,
                     vf_UG,
                     vf_Desp_A,
                     fv_fugas,
                     fv_falta,
                     rep(1,length(fv_falta)),
                     vf_scarcity_residents,
                     rep(1,length(fv_falta)), #flooding do not influence protests or water capture
                     vf_H)
  
  #house modification flooding
  C_R_D<-cbind(vf_WQ,
               vf_UG,
               rep(1,length(fv_falta)),
               rep(1,length(fv_falta)),
               fv_falta,
               vf_garbage,
               rep(1,length(fv_falta)), #scarcity does not affect floding
               vf_pond, 
               vf_H)
  
  
  ################################################################################################################
  #2)calculate distance for each census block for action mantainance and build new infrastructure
  distance_ideal_A1_D<-sweep(as.matrix(all_C_D),MARGIN=2,as.vector(Criteria_sacmcx_D)/sum(as.vector(Criteria_sacmcx_D)),FUN=ideal_distance,z=alternative_weights_D[1]/sum(alternative_weights_D)) #"Mantenimiento"
  distance_ideal_A2_D<-sweep(as.matrix(all_C_D),MARGIN=2,as.vector(Criteria_sacmcx_D)/sum(as.vector(Criteria_sacmcx_D)),FUN=ideal_distance,z=alternative_weights_D[2]/sum(alternative_weights_D)) # "Nueva_infraestructura"
  
  distance_ideal_A1_Ab<-sweep(as.matrix(all_C_ab),MARGIN=2,as.vector(Criteria_sacmcx_Ab)/sum(as.vector(Criteria_sacmcx_Ab)),FUN=ideal_distance,z=alternative_weights_S[4]/sum(alternative_weights_S[c(4,5)]))# "Mantenimiento"
  distance_ideal_A2_Ab<-sweep(as.matrix(all_C_ab),MARGIN=2,as.vector(Criteria_sacmcx_Ab)/sum(as.vector(Criteria_sacmcx_Ab)),FUN=ideal_distance,z=alternative_weights_S[5]/sum(alternative_weights_S[c(4,5)]))# "Nueva_infraestructura"
  
  #Residents
  #distance_ideal_protest<-sweep(as.matrix(C_R_protest[,-c(6,8)]),MARGIN=2,as.vector(Criteria_residents_Iz[-c(6,8)])/sum(as.vector(Criteria_residents_Iz[-c(6,8)])),FUN=ideal_distance,z=alternative_weights_Iz[5]/sum(alternative_weights_Iz[c(4,5)]))# "Protests"
  distance_ideal_protest= 1 - vf_scarcity_residents
  distance_ideal_House_mod_lluvia<-sweep(as.matrix(C_R_D[,-c(3,4,7)]),MARGIN=2,as.vector(Criteria_residents_Iz[-c(3,4,7)])/sum(as.vector(Criteria_residents_Iz[-c(3,4,7)])),FUN=ideal_distance,z=alternative_weights_Iz[4]/sum(alternative_weights_Iz[c(4,5)]))# "House modification"
  distance_ideal_House_mod_agua<-sweep(as.matrix(C_R_HM[,-c(6,8)]),MARGIN=2,as.vector(Criteria_residents_Iz[-c(6,8)])/sum(as.vector(Criteria_residents_Iz[-c(6,8)])),FUN=ideal_distance,z=alternative_weights_Iz[4]/sum(alternative_weights_Iz[c(4,5)]))# "House modification"
  ################################################################################################################
  #3) save value function and distance matrix as a shape file
  Output_value_function<-study_area_cvg
  Output_value_function@data<-cbind(Output_value_function@data,all_C_D,all_C_ab,distance_ideal_A1_D,distance_ideal_A2_D,distance_ideal_A1_Ab,distance_ideal_A2_Ab,distance_ideal_House_mod_lluvia,distance_ideal_House_mod_lluvia,distance_ideal_House_mod_agua)
  
  list(distance_ideal_A1_D=distance_ideal_A1_D,
       distance_ideal_A2_D=distance_ideal_A2_D,
       distance_ideal_A1_Ab=distance_ideal_A1_Ab,
       distance_ideal_A2_Ab=distance_ideal_A2_Ab,
       distance_ideal_protest=distance_ideal_protest,
       distance_ideal_House_mod_lluvia=distance_ideal_House_mod_lluvia,
       distance_ideal_House_mod_agua=distance_ideal_House_mod_agua,
       Output_value_function=Output_value_function)
}