update_flooding_index<-function(study_data){

 #q100
 #f_prec_v
 #f_esc
 #historic_flooding_freq VF
 #runnoff_yesNo

#need to define min and max and these will not change over time
#f_prec_v:

fv_f_prec_v<-sapply(
  study_data$f_prec_v,
  FUN=convexa_decreciente,
   xmax=  8930363.15853,# [mm/km2]==  1202 mm/year
   xmin=10590.85,# [mm/km2]
  gama= 0.035)

fv_non_potable_capacity<-sapply(
  study_data$non_potable_capacity,
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

fv_historic_flooding_freq<-sapply(
  study_data$inunda,
  FUN= logistic_invertida,
  xmax= 8.0266,
  xmin= 0,
  k=0.083,
  center=4.013)# min+(max-min)/2 ==49


#calculate weights for each factor
#For now, weights are equal for all the factors: 1/3 for areas without runoff and
#1/4 for areas with runoff

  w_historic_flooding_freq=1/(study_data$runoff_bin+3)
  w_f_prec_v=1/(study_data$runoff_bin+3)
  w_fv_non_potable_capacity=1/(study_data$runoff_bin+3)
  w_f_esc=study_data$runoff_bin/(study_data$runoff_bin+3)



  flooding_index=(w_historic_flooding_freq*fv_historic_flooding_freq) +
                (w_f_prec_v*fv_f_prec_v) +
                (w_fv_non_potable_capacity*fv_non_potable_capacity)+
                (w_f_esc*fv_f_esc)

  tibble::tibble(
    ageb_id = study_data$ageb_id,
    flooding_index = flooding_index) #crear variable en dataframe


}

flooding_multicriteria_index_component <- list(
  initialize = function(study_data, value_function_config) {
    study_data %>%
      dplyr::inner_join(update_flooding_index(study_data = study_data), by = PK_JOIN)
  },
  transition = update_flooding_index
)
