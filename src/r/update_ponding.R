#update the number of flooding events

#flooding_update<-predict(fit_zinbinom,newdata=studyArea_CVG@data,type='response')
#studyArea_CVG@data$encharca <-flooding_update


#
#from regression tree
## Se hace la prediccion para cada zona (1:9)
for(hh in 1:9){
studyArea_CVG@data$encharca[which(studyArea_CVG@data$region==hh)]<-predict(Modelos[[hh]],
                                                                           subset(studyArea_CVG@data,select=c("f_prec_v", "f_esc", "n_tramos", "q100", "bombeo_tot", "rejillas"))[which(studyArea_CVG@data$region==hh),], # observaciones de todas las variables de la region
                                                                            n.trees = 9566, # Número de árboles que usa el modelo 
                                                                            type = "response")
}
print(studyArea_CVG@data$encharca[1708])   
#for ageb 1708 with AGEB_ID =6420 the value of ponding is 10 time larger than the average
#check


subset(studyArea_CVG@data,select=c("f_prec_v", "f_esc", "n_tramos", "q100", "bombeo_tot", "rejillas"))[1708,]
