#Second version of cycle for VBSA

run<-function(x) {
  
  #Setup step
  source("r/VBSAsetup.R")
  
  #Assign values to variables
  effectivity_newInfra<<-x[1]
  effectivity_mantenimiento<<-x[2]
  decay_infra<<-x[3]
  Budget<<-x[4]
  
  for (i in 1:length(ini_date)){
    source("r/VBSAscarcity_update.R")
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
      source("r/read_Climate_scenarios.R")
      source("r/update_ponding.R")
      #run Health model
      
      #run Site suitability
      source("r/site_suitability.R")
      #run Site selection
      source("r/site_selection.R")
      #take actions sacmex
      source("r/take_actions_sacmex.R")
      #update the level of adaptation and sensitivity of residents
      source("r/take_actions_residents.R")
      source("r/adaptation_and_sensitivity.R")
      #Update age and condition of infrastructure
      source("r/update_age_infrastructure.R")
      #Save results
      
      TS_res<-save_TS(TR = i,result_prev_time=TS_res,year=year_ts[i],month=month_ts[i])
    }
    #update number of protests
    source("r/protests.R")
  }
  lastT<-max(TS_res$time_sim)
  Vlast<-subset(TS_res,time_sim==lastT,select=c(vulnerability_Ab,vulnerability_D))
  return(Vlast)
}
