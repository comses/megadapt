update_climate <- function(study_data, climate_scenario) {
  # create a vector of alternative years for sampling
  # and sample one of them each year
  year_sampled_from_Scenario <- sample(size = 1, x = 1993:2013)

  # subset the data.frame of scenario for the year sampled
  sampled_rain_runoff_scenario <- subset(climate_scenario, year == year_sampled_from_Scenario)

  # Every year of simulation, sample with a uniform distribution a single year of estimations from the N columns posible (2001-20013?)

  # make a match between the ID of agebs from the full data set
  # and the data.frame from the scenario

  match_values <- match(study_data$ageb_id, sampled_rain_runoff_scenario$ageb_id)

  # Replace volume of rainfall
  study_data$f_prec_v <- sampled_rain_runoff_scenario$prec[match_values]
  # Replace runoff
  study_data$f_esc <- sampled_rain_runoff_scenario$runoff[match_values]
  study_data
}
