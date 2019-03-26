#' Create a model estimating the relationship between days without clean water
#' and water infrastructure age
#'
#' @param study_data A data frame with the needed predictor variables
#' @return A model to predict future clean water availability given census block attributes
create_water_scarcity_model <- function(study_data) {
  pscl::zeroinfl(lambdas ~ critic_z + antiguedad_dist | falta_dist,
                 dist = "negbin",
                 data = study_data)
}



#' Generate random water scarcity data for a year for all census blocks
#'
#' @param water_scarcity_model A model capable of predicting water scarcity probabilities by census block
#' @param study_data Census block data
#'   \describe{
#'   \item{antiguedad_dist}{FPotable water infrastructure age in years}
#'   \item{critic_z}{Indicator variable of whether census block has less than 4 hours of water per day}
#'   \item{falta_dist}{Proportion of houses in census block without water}
#'   }
#' @param week_of_year The number of weeks up until the present week of the current year
#' @param value_function_config A set of functions to modify the scarcity index
#' @return data frame with pk and cumulative number of days without clean water this week and year by census block
update_water_scarcity <-
  function(water_scarcity_model,
           study_data,
           week_of_year,
           value_function_config) {
    prob_water <-
      predict(water_scarcity_model, newdata = study_data, type = "prob", at=0:7)

    days_wn_water_week <- max.col(t(apply(
      prob_water,
      1,
      FUN = function(prob) rmultinom(n = 1, prob = prob, size = 1)
    ))) - 1

    if (week_of_year == 1) {
      days_wn_water_year <- days_wn_water_week
    } else {
      days_wn_water_year <- study_data$days_wn_water_year + days_wn_water_week
    }
    #update scarcity index here
    scarcity_index<-update_scarcity_index(study_data,value_function_config=value_function_config)

    study_data %>%
      dplyr::mutate(
        days_wn_water_two_weeks = days_wn_water_week + (!! days_wn_water_week),
        days_wn_water_week = (!! days_wn_water_week),
        days_wn_water_year = (!! days_wn_water_year)
      ) %>%
      dplyr::select(
        ageb_id,
        days_wn_water_week,
        days_wn_water_two_weeks,
        days_wn_water_year,
        scarcity_index
      )
  }

update_water_scarcity_original <- function(study_area_cvg, water_scarcity_model) {
  # update water scarcity  model
  # generate a new prediction
  prob_water <- predict(water_scarcity_model, newdata = study_area_cvg@data, type = "prob")
  # generate the lottery
  prob_NOPRoblemwater<-predict(water_scarcity_model,newdata=study_area_cvg@data,type="zero")

  water_no <- rbinom(n = length(prob_water[, 7]), size = 1, prob = prob_water[, 7]) * 7
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 6]), size = 1, prob = prob_water[which(water_no == 0), 6]) * 6
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 5]), size = 1, prob = prob_water[which(water_no == 0), 5]) * 5
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 4]), size = 1, prob = prob_water[which(water_no == 0), 4]) * 4
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 3]), size = 1, prob = prob_water[which(water_no == 0), 3]) * 3
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 2]), size = 1, prob = prob_water[which(water_no == 0), 2]) * 2
  water_no[which(water_no == 0)] <- rbinom(n = length(prob_water[which(water_no == 0), 1]), size = 1, prob = prob_water[which(water_no == 0), 1]) * 1


  water_no<-water_no*(prob_NOPRoblemwater>runif(n = length(water_no)))

  # Here the accumulation of two weeks without water (are accumualted)
  study_area_cvg@data$NOWater_twoweeks <- study_area_cvg@data$NOWater_week_pois + water_no
  # update value of days with not water in a week
  study_area_cvg@data$NOWater_week_pois <- water_no
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
