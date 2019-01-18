#read results
#Agregated results by delegations
#Agregated results by census block
#agregate data by city
#maps
#time series

#save results
save_TS<-function(TR,result_prev_time,month,year){
res<-rbind(result_prev_time,cbind(subset(studyArea_CVG@data,select = var_selected),
                          time_sim=rep(TR,length(studyArea_CVG@data$AGEB_ID)),
                          month_sim=rep(month,length(studyArea_CVG@data$AGEB_ID)),
                          year_sim=rep(year,length(studyArea_CVG@data$AGEB_ID))))
return(res)
}
