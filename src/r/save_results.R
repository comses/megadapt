#read results
#Agregated results by delegations
#Agregated results by census block
#agregate data by city
#maps
#time series

#save results
save_TS <- function(study_area_cvg, TR, result_prev_time, month, year) {
  res <-
    rbind(result_prev_time,
          cbind(
            subset(study_area_cvg@data, select = var_selected),
            time_sim = rep(TR, length(study_area_cvg@data$AGEB_ID)),
            month_sim = rep(month, length(study_area_cvg@data$AGEB_ID)),
            year_sim = rep(year, length(study_area_cvg@data$AGEB_ID))
          ))
  return(res)
}
