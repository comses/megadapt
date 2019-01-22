#Decision cycle
#simulate a yearly cycle of the model by week


for (i in 1:length(ini_date)){
    source("scarcity_update.R")
  
    if (year_change[i]==1){
     source("read_Climate_scenarios.R")
     source("update_ponding.R")
     #run Health model
  
     #run Site suitability
     source("site_suitability.R")
     #run Site selection
     source("site_selection.R")
     #take actions sacmex
     source("take_actions_sacmex.R")
     #update the level of adaptation and sensitivity of residents
     source("take_actions_residents.R")
     source("adaptation_and_sensitivity.R")
     #Update age and condition of infrastructure
     source("update_age_infrastructure.R")
    #Save results
      
      TS_res<-save_TS(TR = i,result_prev_time=TS_res,year=year_ts[i],month=month_ts[i])
    }
  #update number of protests
  source("protests.R")
}



