create_study_data <- function(study_data) {
  study_data$antiguedad_D <- study_data$antiguedad
  study_data$antiguedad_Ab <- study_data$antiguedad

  # water scarcity (week, month year)
  study_data$days_wn_water_year <- 0L
  study_data$days_wn_water_month <- 0L

  # save water scarcity, protests and social pressure
  study_data$social_pressure <- 0L

  # save variables associated with adaptation
  study_data$house_modifications_Ab <- 0L
  study_data$house_modifications_D <- 0L
  # sensitivity of neighborhoods to scarcity and flooding
  study_data$sensitivity_Ab <- 1
  study_data$sensitivity_D <- 1

  # Vulnerability of populations
  study_data$vulnerability_Ab <- 1
  study_data$vulnerability_D <- 1

  # Interventions from water authority
  study_data$Interventions_D <- 1
  study_data$Interventions_Ab <- 1

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

#' Run the megadapt model one year
update_year_megadapt <- function(megadapt, month_step_counts) {
  n_weeks <- sum(month_step_counts$n_weeks)
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
  study_data <-
    update_protests(
      study_data = study_data,
      resident_actions = resident_actions,
      n_weeks_in_year = n_weeks
    )
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
  }

  megadapt$study_area@data <- study_data
  megadapt
}

create_time_info <- function(n_steps) {
  ini_date <- seq.Date(from = as.Date("2019/1/1"),
                       to = as.Date(sprintf("20%s/1/1", (19 + n_steps))),
                       by = "week")
  year_ts <- format(as.Date(ini_date), "%Y")
  year_change_indices <-
    which(as.logical(c(1, diff(
      as.numeric(year_ts)
    ))))
  years <- unique(year_ts)
  month_ts <- format(as.Date(ini_date), '%m')

  month_counts <- tibble::tibble(year = as.integer(year_ts),
                                 month = as.integer(month_ts)) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(n_weeks = n())

  month_counts
}

#' Run the megadapt model
simulate_megadapt <- function(megadapt) {
  all_month_step_counts <- create_time_info(megadapt$params$n_steps)
  years <- sort(unique(all_month_step_counts$year))
  results <- initial_state(megadapt$study_area@data)

  year_index <- 1
  for (current_year in years) {
    month_step_counts <- all_month_step_counts %>% dplyr::filter(year == current_year)
    megadapt <-
      update_year_megadapt(megadapt = megadapt, month_step_counts = month_step_counts)

    results <-
      save_TS(
        study_data = megadapt$study_area@data,
        TR = year_index,
        result_prev_time = results,
        year = current_year,
        month = 12
      )
    year_index <- year_index + 1
  }

  results
}
