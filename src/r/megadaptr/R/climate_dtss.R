#' Load a climate scenario csv given at a particular path
#'
#' @param path Path to the file
#' @return climate scenario data.frame
load_climate_scenario_from_path <- function(path) {
  readr::read_csv(path)
}
load_climate_scenario_from_index <- function(id = 1) {
  index <- readr::read_csv(data_dir("climate_landuse_scenarios/index.csv"))
  row <- index %>% dplyr::filter(id == !! id)
  path <- data_dir("climate_landuse_scenarios",row$path[1])
  readr::read_csv(path)
}


load_climate_scenario <- function(emissions_scenario = 8.5, urban_scenario = "bau", time_horizon = "far_future") {
  index <- readr::read_csv(data_dir("climate_landuse_scenarios/index.csv"))
  row <- index %>% dplyr::filter(emissions_scenario == !! emissions_scenario, urban_scenario == !! urban_scenario, time_horizon == !! time_horizon)
  path <- data_dir("climate_landuse_scenarios",row$path[1])
  readr::read_csv(path)
}

#' Climate a climate scenario model
#'
#' @param id the path to a climate scenario csv
climate_fnss_create <- function(id = 1) {
  climate_scenario <- load_climate_scenario_from_index(id)
  prepend_class(climate_scenario, 'climate_fnss')
}

#' Generates yearly precipitation and runoff (in millimeters) by census block
#'
#' @inheritParams call_fnss
#' @param study_data A data frame of study area data
#' @return yearly runoff and precipitation values by census block
call_fnss.climate_fnss <- function(fnss, study_data) {
  # create a vector of alternative years for sampling
  # and sample one of them each year
  year_sampled_from_scenario <- sample(size = 1, x = 1993:2013)

  # subset the data.frame of scenario for the year sampled
  sampled_rain_runoff_scenario <- subset(fnss, year == year_sampled_from_scenario)

  # Every year of simulation, sample with a uniform distribution a single year of estimations from the N columns posible (2001-20013?)

  # make a match between the ID of agebs from the full data set
  # and the data.frame from the scenario

  match_values <- match(study_data$censusblock_id, sampled_rain_runoff_scenario$censusblock_id)

  tibble::tibble(
    censusblock_id=study_data$censusblock_id,
    precipitation_volume=sampled_rain_runoff_scenario$prec[match_values],
    runoff_volume=sampled_rain_runoff_scenario$runoff[match_values]
  )
}

climate_initialize <- function(climate_fnss, study_data) {
  study_data %>%
    dplyr::inner_join(call_fnss(climate_fnss, study_data), by = PK_JOIN_EXPR)
}
