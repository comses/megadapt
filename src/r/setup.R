library("pscl")

source("r/adaptation_and_sensitivity.R")
source("r/protests.R")
source("r/save_results.R")
source("r/site_selection.R")
source("r/site_suitability.R")
source("r/take_actions_residents.R")
source("r/take_actions_sacmex.R")
source("r/update_age_infrastructure.R")
source("r/update_climate.R")
source("r/update_ponding.R")

create_study_data <- function(study_data) {
  study_data$antiguedad_D <- study_data$antiguedad
  study_data$antiguedad_Ab <- study_data$antiguedad
  
  # water scarcity (week, month year)
  study_data$days_wn_water_year <-
    rep(0, length(study_data$AGEB_ID))
  study_data$days_wn_water_month <-
    rep(0, length(study_data$AGEB_ID))
  study_data$NOWater_week_pois <- rep(0, length(study_data$AGEB_ID))
  # here we calculate the consecutive accumulation of days without water
  # If this accumulation surpass a threshold, a protest is triggered and social pressure accumulated
  
  # save water scarcity, protests and social pressure
  study_data$NOWater_twoweeks <- numeric(length(study_data$AGEB_ID))
  study_data$protesta <- numeric(length(study_data$AGEB_ID))
  study_data$social_pressure <- numeric(length(study_data$AGEB_ID))
  
  # save variables associated with adaptation
  study_data$house_modifications_Ab <-
    rep(0, length(study_data$AGEB_ID))
  study_data$house_modifications_D <-
    rep(0, length(study_data$AGEB_ID))
  # sensitivity of neighborhoods to scarcity and flooding
  study_data$sensitivity_Ab <- rep(1, length(study_data$AGEB_ID))
  study_data$sensitivity_D <- rep(1, length(study_data$AGEB_ID))
  
  # Vulnerability of populations
  study_data$vulnerability_Ab <- rep(1, length(study_data$AGEB_ID))
  study_data$vulnerability_D <- rep(1, length(study_data$AGEB_ID))
  
  # Interventions from water authority
  study_data$Interventions_D <- rep(1, length(study_data$AGEB_ID))
  study_data$Interventions_Ab <- rep(1, length(study_data$AGEB_ID))
  
  study_data
}

initial_state <- function(study_data) {
  cbind(
    subset(study_data, select = COLUMNS_TO_SAVE),
    time_sim = rep(0, length(study_data$AGEB_ID)),
    year_sim = rep(2018, length(study_data$AGEB_ID)),
    month_sim = rep(12, length(study_data$AGEB_ID))
  )
}

create_water_scarcity_model <- function(study_data) {
  zeroinfl(lambdas ~ CRITICO + antiguedad_Ab |
             V_SAGUA,
           dist = "negbin",
           data = study_data)
}

update_water_scarcity <-
  function(water_scarcity_model, study_data) {
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
    
    # update value of days with not water in a week
    study_data$NOWater_week_pois <- water_yes
    # update value of days with not water in a month
    study_data$days_wn_water_month <- study_data$NOWater_week_pois
    # update value of days with not water in a year
    study_data$days_wn_water_year <- study_data$NOWater_week_pois
    study_data
  }

load_obj <- function(path) {
  env <- new.env()
  nm <- load(path, envir = env)[1]
  env[[nm]]
}

load_ponding_models <- function(base_path) {
  models <- list()
  for (i in 1:9) {
    path <- paste0(base_path, "/encharcamientos/mod_en_reg", i, ".rda")
    models[[i]] <- load_obj(path)
  }
  models
}

create_mental_models <- function(mm_water_operator_d_lim,
                                 mm_water_operator_s_lim,
                                 mm_iz) {
  # name criteria
  names_criteria_sacmex_s <-
    colnames(mm_water_operator_s_lim)[-c(1:5)]
  names_criteria_sacmex_d <-
    colnames(mm_water_operator_d_lim)[-c(1, 2)]
  
  # criteria values
  criteria_sacmcx_ab <- mm_water_operator_s_lim$Antiguedad[-c(1:5)]
  criteria_sacmcx_d <- mm_water_operator_d_lim$Antiguedad[-c(1:2)]
  
  # alternative names
  names_alternative_sacmex_s <-
    colnames(mm_water_operator_s_lim)[c(1:5)]
  names_alternative_sacmex_d <-
    colnames(mm_water_operator_d_lim)[c(1, 2)]
  
  alternative_weights_s <-
    mm_water_operator_s_lim$Antiguedad[c(1:5)]
  alternative_weights_d <-
    mm_water_operator_d_lim$Antiguedad[c(1:2)]
  
  names_criteria_resident_iz <- colnames(mm_iz)[-c(1:5)]
  names_alternative_Resident_iz <- colnames(mm_iz)[c(1:5)]
  
  criteria_residents_iz <- mm_iz$Compra.de.agua[-c(1:5)]
  alternative_weights_iz <- mm_iz$Compra.de.agua[c(1:5)]
  
  list(
    sacmcx = list(
      criteria = list(ab = criteria_sacmcx_ab,
                      d = criteria_sacmcx_d),
      alternative_weights = list(d = alternative_weights_d,
                                 s = alternative_weights_s)
    ),
    residents = list(
      criteria = list(iz = criteria_residents_iz),
      alternative_weights = list(iz = alternative_weights_iz)
    )
  )
}

load_value_function_config <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
  params <- as.list(df %>% tidyr::spread(V1, V2))
  param_names <- names(params)
  
  numeric_keys <- c('a', 'center', 'gama', 'k', 'min', 'max')
  for (numeric_key in numeric_keys) {
    if (numeric_key %in% param_names) {
      params[[numeric_key]] <- as.numeric(params[[numeric_key]])
    }
  }
  
  if ('show_map' %in% param_names) {
    params[['show_map']] <- as.logical(params[['show_map']])
  }
  
  params
}

create_value_function_config <- function(sewer_age,
                                         shortage_age,
                                         salt_water_quality,
                                         shortage_failures,
                                         hours_of_service_failure,
                                         hydraulic_pressure_failure,
                                         subsidence) {
  list(
    sewer_age = sewer_age,
    shortage_age = shortage_age,
    salt_water_quality = salt_water_quality,
    shortage_failures = shortage_failures,
    hours_of_service_failure = hours_of_service_failure,
    hydraulic_pressure_failure = hydraulic_pressure_failure,
    subsidence = subsidence
  )
}

load_climate_scenario <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, header = FALSE)
}

create_params <-
  function(new_infrastructure_effectiveness_rate = 0.07,
           maintenance_effectiveness_rate = 0.07,
           n_steps = 5,
           infrastructure_decay_rate = 0.01,
           budget = 1800,
           half_sensitivity_ab = 10,
           half_sensitivity_d = 10) {
    list(
      new_infrastructure_effectiveness_rate = new_infrastructure_effectiveness_rate,
      maintenance_effectiveness_rate = maintenance_effectiveness_rate,
      n_steps = n_steps,
      infrastructure_decay_rate = infrastructure_decay_rate,
      budget = budget,
      half_sensitivity_ab = half_sensitivity_ab,
      half_sensitivity_d = half_sensitivity_d
    )
  }

create_megadapt <- function(climate_scenario,
                            mental_models,
                            ponding_models,
                            params,
                            study_area,
                            water_scarcity_model,
                            value_function_config) {
  list(
    climate_scenario = climate_scenario,
    mental_models = mental_models,
    params = params,
    ponding_models = ponding_models,
    study_area = study_area,
    water_scarcity_model = water_scarcity_model,
    value_function_config = value_function_config
  )
}

update_year_megadapt <- function(megadapt, n_weeks) {
  climate_scenario <- megadapt$climate_scenario
  mental_models <- megadapt$mental_models
  params <- megadapt$params
  ponding_models <- megadapt$ponding_models
  study_data <- megadapt$study_area@data
  water_scarcity_model <- megadapt$water_scarcity_model
  value_function_config <- megadapt$value_function_config
  
  study_data <-
    update_water_scarcity(study_data = study_data,
                          water_scarcity_model = water_scarcity_model)
  
  study_data <-
    update_climate(study_data = study_data, climate_scenario = climate_scenario)
  study_data <-
    update_ponding(study_data = study_data, ponding_models = ponding_models)
  
  # run site suitability
  site_suitability <-
    determine_site_suitability(
      study_data = study_data,
      mental_models = mental_models,
      value_function_config = value_function_config
    )
  # run site selection
  site_selection <-
    determine_site_selection(site_suitability, budget = params$budget)
  
  # take actions sacmex
  study_data <-
    take_actions_sacmex(params = params,
                        study_data = study_data,
                        site_selection = site_selection)
  
  # update the level of adaptation and sensitivity of residents
  resident_actions <- take_actions_residents(site_suitability)
  if (length(resident_actions$agebs_que_protestan) > 0) {
    study_data <-
      update_protests(
        study_data = study_data,
        resident_actions = resident_actions,
        year_changed = TRUE
      )
  }
  study_data <-
    update_adaptation_and_sensitivity(
      study_data = study_data,
      resident_actions = resident_actions,
      params = params
    )
  # Update age and condition of infrastructure
  study_data <-
    update_infrastructure_age(
      study_data = study_data,
      infrastructure_decay_rate = params$infrastructure_decay_rate
    )
  
  for (week_index in seq(n_weeks - 1)) {
    study_data <-
      update_water_scarcity(study_data = study_data,
                            water_scarcity_model = water_scarcity_model)
    study_data <-
      update_protests(
        study_data = study_data,
        resident_actions = resident_actions,
        year_changed = FALSE
      )
  }
  
  megadapt$study_area@data <- study_data
  megadapt
}

create_time_info <- function(n_steps) {
  ini_date <- seq.Date(from = as.Date("2019/1/1"),
                       to = as.Date(sprintf("20%s/1/1", (19 + n_steps))),
                       by = "week")
  year_ts <- format(as.Date(ini_date), "%Y")
  
  n_weeks <- length(ini_date)
  year_change_indices <-
    which(as.logical(c(1, diff(
      as.numeric(year_ts)
    ))))
  
  tibble::tibble(start = year_change_indices,
                 end = as.integer(c(year_change_indices[-1] - 1, n_weeks))) %>%
    dplyr::mutate(
      n_weeks = end - start + 1,
      start_date = ini_date[start],
      end_date = ini_date[end]
    )
}

simulate_megadapt <- function(megadapt) {
  time_info <- create_time_info(megadapt$params$n_steps)
  results <- initial_state(megadapt$study_area@data)
  
  for (year_index in seq(nrow(time_info))) {
    megadapt <-
      update_year_megadapt(megadapt = megadapt, n_weeks = time_info$n_weeks[year_index])
    end_date <- time_info$end_date[year_index]
    
    results <-
      save_TS(
        study_data = megadapt$study_area@data,
        TR = year_index,
        result_prev_time = results,
        year = format(end_date, '%Y'),
        month = format(end_date, '%m')
      )
  }
  
  results
}
