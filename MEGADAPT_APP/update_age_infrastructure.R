#update infrastructure related atributes
#update_age_infrastructure
studyArea_CVG@data$antiguedad_D<-studyArea_CVG@data$antiguedad_D+1
studyArea_CVG@data$antiguedad_Ab<-studyArea_CVG@data$antiguedad_Ab+1

#update_capacity of the system
studyArea_CVG@data$capac_w<-studyArea_CVG@data$capac_w *(1- decay_infra)
#update capacity index
#FIDEL
#The proportion of people without infrastructure increases proportionally to 
#the growthof the population in each delegation
studyArea_CVG@data$FALTA_IN=studyArea_CVG@data$FALTA_IN * (1+(1-studyArea_CVG@data$FALTA_IN)*studyArea_CVG@data$pop_growth)
studyArea_CVG@data$falta_dren=studyArea_CVG@data$falta_dren* (1+(1-studyArea_CVG@data$falta_dren)*studyArea_CVG@data$pop_growth)
