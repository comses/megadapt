source("r/take_actions_residents.R")
source("r/protests.R")

#Decision cycle
#simulate a yearly cycle of the model by week


for (i in 1:length(ini_date)) {
  source("r/scarcity_update.R")
  
  if (year_change[i] == 1) {
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
    agebs_que_protestan <- take_actions_residents(site_suitability)
    if (length(agebs_que_protestan)>0){
      studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, agebs_que_protestan = agebs_que_protestan)
    }
    source("r/adaptation_and_sensitivity.R")
    #Update age and condition of infrastructure
    source("r/update_age_infrastructure.R")
    #Save results
    
    TS_res <-
      save_TS(
        TR = i,
        result_prev_time = TS_res,
        year = year_ts[i],
        month = month_ts[i]
      )
  }
  #update number of protests
  studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, agebs_que_protestan = agebs_que_protestan)
}
