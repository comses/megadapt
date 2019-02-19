source("r/adaptation_and_sensitivity.R")
source("r/protests.R")
source("r/scarcity_update.R")
source("r/site_selection.R")
source("r/site_suitability.R")
source("r/take_actions_residents.R")
source("r/take_actions_sacmex.R")
source("r/update_age_infrastructure.R")
source("r/update_climate.R")
source("r/update_ponding.R")

# Decision cycle
# simulate a yearly cycle of the model by week


for (i in 1:length(ini_date)) {
  studyArea_CVG <- update_water_scarcity(study_area_cvg = studyArea_CVG, water_scarcity_model = water_scarcity_model)

  if (year_change[i] == 1) {
    studyArea_CVG <- update_climate(study_area_cvg = studyArea_CVG, s_85 = S_85)
    studyArea_CVG <- update_ponding(study_area_cvg = studyArea_CVG, ponding_models = Modelos)
    # run Health model

    # run Site suitability
    site_suitability <- determine_site_suitability(study_area_cvg = studyArea_CVG)
    # run Site selection
    site_selection <- determine_site_selection(site_suitability)

    # take actions sacmex
    studyArea_CVG <- take_actions_sacmex(study_area_cvg = studyArea_CVG, site_selection = site_selection)

    # update the level of adaptation and sensitivity of residents
    resident_actions <- take_actions_residents(site_suitability)
    if (length(resident_actions$agebs_que_protestan) > 0) {
      studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
    }
    studyArea_CVG <- update_adaptation_and_sensitivity(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
    # Update age and condition of infrastructure
    studyArea_CVG <- update_infrastructure_age(study_area_cvg = studyArea_CVG)

    # Save results

    TS_res <-
      save_TS(
        study_area_cvg = studyArea_CVG,
        TR = i,
        result_prev_time = TS_res,
        year = year_ts[i],
        month = month_ts[i]
      )
  }
  # update number of protests
  studyArea_CVG <- update_protests(study_area_cvg = studyArea_CVG, resident_actions = resident_actions)
}
