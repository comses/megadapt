#update the number of flooding events

#flooding_update<-predict(fit_zinbinom,newdata=studyArea_CVG@data,type='response')
#studyArea_CVG@data$encharca <-flooding_update


#
#from regression tree
## Se hace la prediccion para cada zona (1:9)
update_ponding <- function(study_area_cvg, ponding_models) {
  for (hh in 1:9) {
    study_area_cvg@data$encharca[which(study_area_cvg@data$region == hh)] <-
      predict(
        ponding_models[[hh]],
        subset(
          study_area_cvg@data,
          select = c(
            "f_prec_v",
            "f_esc",
            "n_tramos",
            "q100",
            "bombeo_tot",
            "rejillas"
          )
        )[which(study_area_cvg@data$region == hh), ],
        # observaciones de todas las variables de la region
        n.trees = 9566,
        # Número de árboles que usa el modelo
        type = "response"
      )
  }
  print(study_area_cvg@data$encharca[1708])
  #for ageb 1708 with AGEB_ID =6420 the value of ponding is 10 time larger than the average
  #check
  
  
  subset(
    study_area_cvg@data,
    select = c(
      "f_prec_v",
      "f_esc",
      "n_tramos",
      "q100",
      "bombeo_tot",
      "rejillas"
    )
  )[1708, ]
  
  study_area_cvg
}