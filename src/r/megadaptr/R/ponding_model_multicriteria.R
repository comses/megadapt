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
  FUN= concava_creciente,
  xmax= 2064.34,
  xmin= 0,
  gamma=0.049)


fv_f_esc<-sapply(
  study_data$f_esc,
  FUN= convexa_decreciente,
  xmax= 504,
  xmin= 0,
  gama=0.035)

fv_historic_ponding_freq<-sapply(
  study_data$encharca,
  FUN= logistic_invertida,
  xmax= 97.92955,
  xmin= 0,
  k=0.083,
  center=49)# min+(max-min)/2 ==49


  #if(study_data$runnoff_yesNo==1){
  w_f_prec_v=1/4
  w_q100=1/4
  w_f_esc=1/4
  w_historic_ponding_freq=1/4
  encharca_index=(w_historic_ponding_freq*fv_historic_ponding_freq) +
                (w_f_prec_v*fv_f_prec_v) +
                (w_q100*fv_q100)+
                (w_f_esc*fv_f_esc)

#  }
  #else{
  #w_f_prec_v=1/3
  #w_q100=1/3
  #w_historic_ponding_freq=1/3
  #pooding_index=(w_historic_ponding_freq*fv_historic_ponding_freq) +
                #(w_f_prec_v*fv_f_prec_v) +
                #(w_q100*fv_q100)
#  }
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


