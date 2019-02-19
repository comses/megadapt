#Second version of cycle for VBSA
source("r/read_Climate_scenarios.R")
source("r/VBSAscarcity_update.R")
source("r/update_ponding.R")
source("r/site_suitability.R")
source("r/site_selection.R")
source("r/take_actions_sacmex.R")
source("r/take_actions_residents.R")
source("r/VBSAprotests.R")
source("r/adaptation_and_sensitivity.R")
source("r/update_age_infrastructure.R")

#setup file for the MEGADAPT model R -version
#load packages
require(glmmADMB)
require(maptools)
require(ecr)
require(pscl)
require(gbm)
#require(ggplot2)
require(plyr) #Tools for Splitting, Applying and Combining Data
#require(ggmap)
#require(ahp) #Analitical hiererquical process package
#require(ahpsurvey)
require(gramEvol) #genetic algorithm optimization
#require(gridExtra)
#require(DT)

run<-function(x) {
  
  #Setup step
  source("r/VBSAsetup.R")
  
  #Assign values to variables
  effectivity_newInfra<<-x[1]
  effectivity_mantenimiento<<-x[2]
  decay_infra<<-x[3]
  Budget<<-x[4]
  
  for (i in 1:length(ini_date)) {
    studyArea_CVG <- update_water_scarcity(study_area_cvg = studyArea_CVG, water_scarcity_model = water_scarcity_model)
    #update value of days with not water in a month
    if(month_change[i]==1){
      studyArea_CVG@data$days_wn_water_month<-studyArea_CVG@data$NOWater_week_pois
    }else{
      studyArea_CVG@data$days_wn_water_month<-studyArea_CVG@data$days_wn_water_month + studyArea_CVG@data$NOWater_week_pois
    }
    #update value of days with not water in a year
    if(year_change[i]==1){studyArea_CVG@data$days_wn_water_year<-studyArea_CVG@data$NOWater_week_pois
    }else{
      studyArea_CVG@data$days_wn_water_year<-studyArea_CVG@data$days_wn_water_year + studyArea_CVG@data$NOWater_week_pois
    }
    
    if (year_change[i]==1){
      studyArea_CVG <- setup_climate_scenarios(study_area_cvg = studyArea_CVG, s_85 = S_85)
      studyArea_CVG <- update_ponding(study_area_cvg = studyArea_CVG, ponding_models = Modelos)
      #run Health model
      
      #run Site suitability
      site_suitability <- determine_site_suitability(study_area_cvg = studyArea_CVG, fv_antiguedad_drenaje = fv_antiguedad_drenaje, logistic_invertida = logistic_invertida)
      #run Site selection
      site_selection <- determine_site_selection(site_suitability)
      
      #take actions sacmex
      studyArea_CVG <- take_actions_sacmex(study_area_cvg = studyArea_CVG, site_selection = site_selection)
      
      #update the level of adaptation and sensitivity of residents
      resident_actions <- take_actions_residents(site_suitability)
      if (length(resident_actions$agebs_que_protestan)>0){
        studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
      }
      studyArea_CVG <- update_adaptation_and_sensitivity(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
      #Update age and condition of infrastructure
      studyArea_CVG <- update_infrastructure_age(study_area_cvg = studyArea_CVG)
      
      #Save results
      
      TS_res <-
        save_TS(
          study_area_cvg = studyArea_CVG,
          TR = i,
          result_prev_time = TS_res,
          year = year_ts[i],
          month = month_ts[i]
        )
    }
    #update number of protests
    studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
    #accumulate protests as social_pressure
    if (year_change[i] == 1) {
      studyArea_CVG@data$social_pressure <- studyArea_CVG@data$protesta
    } else {
      studyArea_CVG@data$social_pressure <- studyArea_CVG@data$social_pressure + studyArea_CVG@data$protesta
    }
  }
  lastT<-max(TS_res$time_sim)
  Vlast<-subset(TS_res,time_sim==lastT,select=c(vulnerability_Ab,vulnerability_D))
  return(Vlast)
}
