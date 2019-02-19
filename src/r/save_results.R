# data table to save output data
# save variables:
# Age infra
# capacity infra
# Number of days in a year without water supply
# flooding events
# Protests
# Adaptations Ab
# Adaptations F
# Vulneability Index
COLUMNS_TO_SAVE <- c(
  "AGEB_ID",
  "municipio",
  "antiguedad_D",
  "antiguedad_Ab",
  "f_en",
  "encharca",
  "FALTA_IN",
  "capac_w",
  "falta_dren",
  "lambdas",
  "NOWater_week_pois",
  "NOWater_twoweeks",
  "days_wn_water_month",
  "days_wn_water_year",
  "social_pressure",
  "sensitivity_Ab",
  "sensitivity_D",
  "vulnerability_Ab",
  "vulnerability_D",
  "Interventions_Ab",
  "Interventions_D"
)

# save results
save_TS <- function(study_data,
                    TR,
                    result_prev_time,
                    month,
                    year) {
  rbind(result_prev_time,
        cbind(
          subset(study_data, select = COLUMNS_TO_SAVE),
          time_sim = rep(TR, length(study_data$AGEB_ID)),
          month_sim = rep(month, length(study_data$AGEB_ID)),
          year_sim = rep(year, length(study_data$AGEB_ID))
        ))
}
