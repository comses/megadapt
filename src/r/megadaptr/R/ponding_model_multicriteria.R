update_ponding_index<-function(study_data){

 #q100
 #f_prec_v
 #f_esc
 #historic_ponding_freq VF: Logistica invertida
 #runnoff_yesNo

#need to define min and max and these will not change over time
#f_prec_v:

fv_f_prec_v<-sapply(
  study_data$f_prec_v,
  FUN=convexa_decreciente,
   xmax=  8930363.15853,# [mm/km2]==  1202 mm/year
   xmin=10590.85,# [mm/km2]
  gama= 0.035)

fv_q100<-sapply(
  study_data$q100,
  FUN= convexa_creciente,
  xmax= 2064.34,
  xmin= 0,
  gama=0.01975)


fv_f_esc<-sapply(
  study_data$f_esc,
  FUN= convexa_decreciente,
  xmax= 504,
  xmin= 0,
  gama=0.035)

fv_historic_ponding_freq<-sapply(
  study_data$prom_en,
  FUN= logistic_invertida,
  xmax= 10,
  xmin= 0,
  k=0.108,
  center=3.5)# min+(max-min)/2 ==49


#calculate weights for each factor
#For now, weights are equal for all the factors: 1/3 for areas without runoff and
#1/4 for areas with runoff

  w_historic_ponding_freq=1/(megadapt$study_area$runoff_bin+3)
  w_f_prec_v=1/(megadapt$study_area$runoff_bin+3)
  w_q100=1/(megadapt$study_area$runoff_bin+3)
  w_f_esc=megadapt$study_area$runoff_bin/(megadapt$study_area$runoff_bin+3)


rowSums(cbind(w_f_prec_v,w_q100,w_f_esc,w_historic_ponding_freq))

  encharca_index=(w_historic_ponding_freq*fv_historic_ponding_freq) +
                (w_f_prec_v*fv_f_prec_v) +
                (w_q100*fv_q100)+
                (w_f_esc*fv_f_esc)

  tibble::tibble(
    ageb_id = study_data$ageb_id,
    encharca_index = encharca_index) #crear variable en dataframe


}

ponding_multicriteria_index_component <- list(
  initialize = function(study_data, value_function_config) {
    study_data %>%
      dplyr::inner_join(update_ponding_index(study_data = study_data), by = PK_JOIN)
  },
  transition = update_ponding_index
)


