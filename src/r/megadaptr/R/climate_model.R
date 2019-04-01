#' Load a climate scenario csv given at a particular path
#'
#' @param path Path to the file
#' @return climate scenario data.frame
load_climate_scenario <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, header = FALSE)
}

#' Generates yearly precipitation and runoff (in millimeters) by census block
#'
#' @param study_data A data frame of study area data
#' @param climate_scenario A data frame containing yearly precipitation and runoff amounts (in millimetres)
#'   per census block
#' @return yearly runoff and precipitation values by census block
update_climate <- function(study_data, next_year_study_data, climate_scenario) {
  # create a vector of alternative years for sampling
  # and sample one of them each year
  year_sampled_from_Scenario <- sample(size = 1, x = 1993:2013)

  # subset the data.frame of scenario for the year sampled
  sampled_rain_runoff_scenario <- subset(climate_scenario, year == year_sampled_from_Scenario)

  # Every year of simulation, sample with a uniform distribution a single year of estimations from the N columns posible (2001-20013?)

  # make a match between the ID of agebs from the full data set
  # and the data.frame from the scenario

  match_values <- match(study_data$ageb_id, sampled_rain_runoff_scenario$ageb_id)

  tibble::tibble(
    ageb_id=study_data$ageb_id,
    f_prec_v=sampled_rain_runoff_scenario$prec[match_values],
    f_esc=sampled_rain_runoff_scenario$runoff[match_values]
  )
}

climate_component <- list(
  initialize = function(study_data) {
    study_data %>%
      dplyr::mutate(f_prec_v=0,
                    f_esc=0)
  },
  transition = update_climate
)
