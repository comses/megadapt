#change the level of adaptation to water scarcity based on the decision "house modification"
#change the sensitivity of the agents to water scarcity after the modification

if (length(HM_LL)>0){
  studyArea_CVG@data$house_modifications_D[HM_LL]=studyArea_CVG@data$house_modifications_D[HM_LL]+1
  studyArea_CVG@data$sensitivity_D[HM_Agua]<-1 -(studyArea_CVG@data$house_modifications_D[HM_Agua]/(hsc_D+studyArea_CVG@data$house_modifications_D[HM_Agua]))
}

#change the level of adaptation to flooding based on the decision "house modification"
#change the sensitivity of the agents to flooding events after the modification

if (length(HM_Agua)>0){
  studyArea_CVG@data$house_modifications_Ab[HM_Agua]=studyArea_CVG@data$house_modifications_Ab[HM_Agua]+1
  studyArea_CVG@data$sensitivity_Ab[HM_Agua]<-1- (studyArea_CVG@data$house_modifications_Ab[HM_Agua]/(hsc_Ab+studyArea_CVG@data$house_modifications_Ab[HM_Agua]))
}

#update vulnerability
studyArea_CVG@data$vulnerability_Ab=(studyArea_CVG@data$sensitivity_Ab * studyArea_CVG@data$days_wn_water_year)/(1+studyArea_CVG@data$ingreso)
studyArea_CVG@data$vulnerability_D=(studyArea_CVG@data$sensitivity_D * studyArea_CVG@data$encharca)/(1+studyArea_CVG@data$ingreso)