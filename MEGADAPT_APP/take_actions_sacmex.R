#take actions sacmex
#cahnge value of atributes in agebs selected for action
#action 1 mantainance D
if(length(A1)>0){
  studyArea_CVG@data$antiguedad_D[A1]=studyArea_CVG@data$antiguedad_D[A1]-studyArea_CVG@data$antiguedad_D[A1]*effectivity_mantenimiento
  studyArea_CVG@data$q100[A1]=studyArea_CVG@data$q100[A1] * (1 + effectivity_mantenimiento)    #capasity of drainage increases with mantainance
  studyArea_CVG@data$Interventions_D[A1]=studyArea_CVG@data$Interventions_D[A1]+1      
    }

#action 2 New infra D
if(length(A2)>0){
  studyArea_CVG@data$falta_dren[A2]=studyArea_CVG@data$falta_dren[A2]-studyArea_CVG@data$falta_dren[A2]*effectivity_newInfra
  studyArea_CVG@data$q100[A2] =studyArea_CVG@data$q100[A2] * (1 + effectivity_mantenimiento)  #capasity of drainage increases with new infrastructure
  studyArea_CVG@data$bombeo_tot[A2]=studyArea_CVG@data$bombeo_tot[A2] + 1  #capasity of drainage increases with new infrastructure
  
  studyArea_CVG@data$Interventions_D[A2]=studyArea_CVG@data$Interventions_D[A2]+1
}

#action 3 mantainance Ab.
if(length(A3)>0){
  studyArea_CVG@data$antiguedad_Ab[A3]=studyArea_CVG@data$antiguedad_Ab[A3]*(1-effectivity_mantenimiento)
  studyArea_CVG@data$Interventions_Ab[A3]=studyArea_CVG@data$Interventions_Ab[A3]+1
}

#action 4 New infra Ab.
if(length(A4)>0){
  studyArea_CVG@data$V_SAGUA[A4]=studyArea_CVG@data$V_SAGUA[A4]*(1-effectivity_newInfra)
  studyArea_CVG@data$Interventions_Ab[A4]=studyArea_CVG@data$Interventions_Ab[A4]+1
  
}

