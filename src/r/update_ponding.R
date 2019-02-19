# update the number of flooding events

# flooding_update<-predict(fit_zinbinom,newdata=studyArea_CVG@data,type='response')
# studyArea_CVG@data$encharca <-flooding_update


#
# from regression tree
## Se hace la prediccion para cada zona (1:9)
update_ponding <- function(study_data, ponding_models) {
  for (hh in 1:9) {
    study_data$encharca[which(study_data$region == hh)] <-
      predict(
        ponding_models[[hh]],
        subset(
          study_data,
          select = c(
            "f_prec_v",
            "f_esc",
            "n_tramos",
            "q100",
            "bombeo_tot",
            "rejillas"
          )
        )[which(study_data$region == hh), ],
        # observaciones de todas las variables de la region
        n.trees = 9566,
        # Número de árboles que usa el modelo
        type = "response"
      )
  }
  print(study_data$encharca[1708])

  study_data
}
