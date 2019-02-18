# statistical model of water scarcity. Developed by Ale Estrada and Yozune Miquelajauregui
library("pscl")

setup_water_scarcity <- function(study_area_cvg) {
  water_scarcity_model <- zeroinfl(lambdas ~ CRITICO + antiguedad_Ab | V_SAGUA, dist = "negbin", data = study_area_cvg@data)

  prob_water <- predict(water_scarcity_model, newdata = study_area_cvg@data, type = "prob")
  water_yes <- rbinom(n = length(prob_water[, 7]), size = 1, prob = prob_water[, 7]) * 7
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 6]), size = 1, prob = prob_water[which(water_yes == 0), 6]) * 6
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 5]), size = 1, prob = prob_water[which(water_yes == 0), 5]) * 5
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 4]), size = 1, prob = prob_water[which(water_yes == 0), 4]) * 4
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 3]), size = 1, prob = prob_water[which(water_yes == 0), 3]) * 3
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 2]), size = 1, prob = prob_water[which(water_yes == 0), 2]) * 2
  water_yes[which(water_yes == 0)] <- rbinom(n = length(prob_water[which(water_yes == 0), 1]), size = 1, prob = prob_water[which(water_yes == 0), 1]) * 1

  # update value of days with not water in a week
  study_area_cvg@data$NOWater_week_pois <- water_yes
  # update value of days with not water in a month
  study_area_cvg@data$days_wn_water_month <- study_area_cvg@data$NOWater_week_pois
  # update value of days with not water in a year
  study_area_cvg@data$days_wn_water_year <- study_area_cvg@data$NOWater_week_pois
  list(water_scarcity_model = water_scarcity_model, study_area_cvg = study_area_cvg)
}

water_scarcity_model_results <- setup_water_scarcity(study_area_cvg = studyArea_CVG)
water_scarcity_model <- water_scarcity_model_results$water_scarcity_model
studyArea_CVG <- water_scarcity_model_results$study_area_cvg
