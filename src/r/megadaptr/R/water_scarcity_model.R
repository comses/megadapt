#' Create a model estimating the relationship between days without clean water
#' and water infrastructure age
#'
#' @param study_data A data frame with the needed predictor variables
#' @return A model to predict future clean water availability given census block attributes
create_water_scarcity_model <- function(study_data) {
  zeroinfl(lambdas ~ CRITICO + antiguedad_Ab |
             V_SAGUA,
           dist = "negbin",
           data = study_data)
}

#' Generate random water scarcity data for a year for all census blocks
#'
#' @param water_scarcity_model A model capable of predicting water scarcity probabilities by census block
#' @param study_data Census block data. The data contains
#'   \describe{
#'   \item{antiguedad_Ab} Fresh water infrastructure age in years
#'   \item{CRITICO} Indicator variable of whether census block has less than 4 hours of water per day
#'   \item{V_SAGUA} Number of houses in census block without water
#'   }
#' @param week_of_year The number of weeks up until the present week of the current year
#' @return data frame with pk and cumulative number of days without clean water this week and year by census block
update_water_scarcity <-
  function(water_scarcity_model, study_data, week_of_year) {
    prob_water <-
      predict(water_scarcity_model, newdata = study_data, type = "prob")
    water_yes <-
      rbinom(n = length(prob_water[, 7]),
             size = 1,
             prob = prob_water[, 7]) * 7
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 6]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 6]) * 6
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 5]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 5]) * 5
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 4]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 4]) * 4
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 3]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 3]) * 3
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 2]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 2]) * 2
    water_yes[which(water_yes == 0)] <-
      rbinom(n = length(prob_water[which(water_yes == 0), 1]),
             size = 1,
             prob = prob_water[which(water_yes == 0), 1]) * 1

    tibble::tibble(
      ageb_id=study_data$ageb_id,
      n_days_no_clean_water_this_week=water_yes,
      n_days_no_clean_water_this_year=study_data$n_days_no_clean_water_this_year + water_yes
    )
  }

update_water_scarcity_original <- function(study_area_cvg, water_scarcity_model) {
  # update water scarcity  model
  # generate a new prediction
  prob_water <- predict(water_scarcity_model, newdata = study_area_cvg@data, type = "prob")
  # generate the lottery
  water_yes <- rbinom(n = length(prob_water[, 7]), size = 1, prob = prob_water[, 7]) * 7
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 6]), size = 1, prob = prob_water[which(water_yes == 0), 6]) * 6
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 5]), size = 1, prob = prob_water[which(water_yes == 0), 5]) * 5
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 4]), size = 1, prob = prob_water[which(water_yes == 0), 4]) * 4
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 3]), size = 1, prob = prob_water[which(water_yes == 0), 3]) * 3
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 2]), size = 1, prob = prob_water[which(water_yes == 0), 2]) * 2
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 1]), size = 1, prob = prob_water[which(water_yes == 0), 1]) * 1

  # Here the accumulation of two weeks without water (are accumualted)
  study_area_cvg@data$NOWater_twoweeks <- study_area_cvg@data$NOWater_week_pois + water_yes
  # update value of days with not water in a week
  study_area_cvg@data$NOWater_week_pois <- water_yes
  # update value of days with not water in a month
  if (month_change[i] == 1) {
    study_area_cvg@data$days_wn_water_month <- study_area_cvg@data$NOWater_week_pois
  } else {
    study_area_cvg@data$days_wn_water_month <- study_area_cvg@data$days_wn_water_month + study_area_cvg@data$NOWater_week_pois
  }
  # update value of days with not water in a year
  if (year_change[i] == 1) {
    study_area_cvg@data$days_wn_water_year <- study_area_cvg@data$NOWater_week_pois
  } else {
    study_area_cvg@data$days_wn_water_year <- study_area_cvg@data$days_wn_water_year + study_area_cvg@data$NOWater_week_pois
  }
  study_area_cvg
}
