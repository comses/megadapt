#function to calculate the scarcity index as implemented by Estrada nd Grave
# from LANCIS

#' Generate scarcity index
#'
#' @param data.frame with variables population_size houses without supply,
#' @return data frame with new population size
update_scarcity_index<-function(study_data,value_function_config){

  sewer_age <- value_function_config$sewer_age
  shortage_age <- value_function_config$shortage_age
  shortage_failures <- value_function_config$shortage_failures
  hydraulic_pressure_failure <- value_function_config$hydraulic_pressure_failure
  subsidence <- value_function_config$subsidence


  fv_pob_ageb <- sapply((study_data$poblacion /study_data$area) * 1000000,
                       FUN = logistic_vf,
                       k = 0.3,
                       xmin = min(study_data$poblacion),
                       xmax = max(study_data$poblacion),
                       center = 15000)

vs=study_area$falta_dist * 1000000
fv_viviendas_sagua<- sapply(vs,
                     FUN = gaussian,
                     a=22,
                     xmin = min(vs),
                     xmax = max(vs),
                     center = abs((min(vs)-max(vs))/2)-1000)

fv_zonas_crit=sapply(study_data$critic_z,
                      FUN=gaussian,
                      a = 23,
                      center = 0,
                      xmin = min(study_data$critic_z),
                      xmax = max(study_data$critic_z))


  0#  zonas criticas no estan en el dataframe
fv_num_cisternas=0  #  study_data$tanks   add cisternas

fv_num_cisternas<-sapply(study_data$tanks,
                         FUN=convexa_creciente,
                         gama= .05,
                         xmax=max(study_data$tanks,na.rm=T),
                         xmin=min(study_data$tanks,na.rm=T)
                         )

fv_dias_sagua=sapply(study_data$wo_water,
                     FUN=gaussian,
                     a = 15,
                     center = 0,
                     xmin = min(study_data$wo_water,na.rm=T),
                     xmax = max(study_data$wo_water,na.rm=T)
                     )


fv_ingreso<- sapply(study_data$income_pc,
                     FUN = convexa_creciente,
                     gama=.015,
                     xmin = min(study_data$income_pc,na.rm = T),
                     xmax = max(study_data$income_pc,na.rm = T))




l=6
ind_esc <- ((1/l)*(fv_pob_ageb) + (1/l)*(fv_zonas_crit) + (1/l)*(fv_num_cisternas) + (1/l)*(fv_ingreso) +  (1/l)*(fv_dias_sagua)  +  (1/l)*(fv_viviendas_sagua))
ind_esc
}
