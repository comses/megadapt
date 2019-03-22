#function to calculate the scarcity index as implemented by Estrada nd Grave
# from LANCIS

#' Generate scarcity index
#'
#' @param data.frame with variables population_size houses without supply,
#' @return data frame with new population size
calculate_scarcity_index<-function(Study_data,value_function_config){
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

fv_zonas_crit=0#  zonas criticas no estan en el dataframe
fv_num_cisternas=0  #add cisternas
fv_dias_sagua=0#definir
vf_income<- sapply(study_data$ingreso,
                     FUN = convexa_creciente,
                     gama=.015,
                     xmin = min(study_data$ingreso,na.rm = T),
                     xmax = max(study_data$ingreso,na.rm = T))

fv_antiguedad <- sapply(study_data$antiguedad_Ab,
                  FUN = campana_invertida,
                  center = shortage_age$center,
                  a = shortage_age$a,
                  xmax = shortage_age$max,
                  xmin = shortage_age$min)

fv_presion_hid <- sapply(study_data$pres_hid,
                          FUN = logistic_vf,
                          k = hydraulic_pressure_failure$k,
                          center = hydraulic_pressure_failure$center,
                          xmax = hydraulic_pressure_failure$max,
                          xmin = hydraulic_pressure_failure$min)

fv_fugas <- sapply(study_data$fugas,
       FUN = gaussian,
       a = 10,
       center = 0,
       xmax = min(study_data$fugas),
       xmin = max(study_data$fugas))

l=8
ind_esc <- ((1/l)*(fv_pob_ageb) + (1/l)*(fv_zonas_crit) + (1/l)*(fv_num_cisternas) + (1/l)*(fv_ingreso) +  (1/l)*(fv_dias_sagua) +  (1/l)*(fv_antiguedad) +  (1/l)*(fv_viviendas_sagua) + (1/l)*(fv_presion_hid) + (1/l)*(fv_fugas))
ind_esc
}
