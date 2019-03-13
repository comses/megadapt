PK_JOIN = c("ageb_id"="ageb_id")

create_study_data <- function(study_data) {
  study_data$antiguedad_D <- study_data$antiguedad
  study_data$antiguedad_Ab <- study_data$antiguedad

  # water scarcity (week, month year)
  study_data$days_wn_water_year <- 0L
  study_data$days_wn_water_month <- 0L
  study_data$days_wn_water_two_weeks <- 0L
  study_data$days_wn_water_week <- 0L
  study_data$water_scarcity_weekly <- rep(list(
    data.frame(
      days_wn_water_week=integer(),
      days_wn_water_two_weeks=integer(),
      days_wn_water_year=integer())),
    nrow(study_data))

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

apply_data_changes <- function(data, changes, join_columns) {
  change_colnames = setdiff(colnames(changes), names(join_columns))

  data %>%
    dplyr::select(-change_colnames) %>%
    dplyr::inner_join(
      changes,
      by = join_columns
    )
}

#' Run the megadapt model one year
update_year_megadapt <- function(megadapt, month_step_counts) {
  n_weeks <- sum(month_step_counts$n_weeks)
  climate_scenario <- megadapt$climate_scenario
  mental_models <- megadapt$mental_models
  params <- megadapt$params
  ponding_models <- megadapt$ponding_models
  study_data <- megadapt$study_area@data %>% dplyr::arrange(ageb_id)
  water_scarcity_model <- megadapt$water_scarcity_model
  value_function_config <- megadapt$value_function_config

  work_plan <- create_public_infrastructure_work_plan(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    budget = params$budget,
    n_weeks = n_weeks
  )

  water_scarcity_weeks <- list()
  this_week_study_data <- study_data
  for (n_week in seq(n_weeks)) {
    site_selection <- work_plan[['data']][[n_week]]
    public_infrastructure_changes <- update_public_infrastructure(
      study_data = this_week_study_data,
      site_selection = site_selection,
      params = params,
      n_weeks = n_weeks
    )
    water_scarcity_changes <- update_water_scarcity(
      water_scarcity_model = water_scarcity_model,
      study_data = this_week_study_data,
      week_of_year = n_week
    )
    social_pressure <- update_protests(
      study_data = this_week_study_data,
      value_function_config = value_function_config,
      mental_models = mental_models,
      week_of_year = n_week
    )
    next_week_changes <- cbind(
      public_infrastructure_changes,
      water_scarcity_changes %>% dplyr::select(-ageb_id),
      social_pressure)

    this_week_study_data <- apply_data_changes(
      this_week_study_data,
      next_week_changes,
      join_columns = PK_JOIN)

    water_scarcity_weeks[[n_week]] <- water_scarcity_changes
  }
  water_scarcity_weeks <- do.call(rbind, water_scarcity_weeks) %>%
    dplyr::group_by(ageb_id) %>%
    dplyr::group_nest(.key='water_scarcity_weekly')
  this_week_study_data <- this_week_study_data %>%
    dplyr::select(-water_scarcity_weekly) %>%
    dplyr::left_join(water_scarcity_weeks, by=PK_JOIN)

  residential_investment_changes <- update_residential_infrastructure_investments(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    params = params
  )

  climate_changes <- update_climate(
    study_data = study_data,
    climate_scenario = climate_scenario
  )

  ponding_changes <- update_ponding(
    study_data = apply_data_changes(
      study_data,
      climate_changes,
      join_columns = PK_JOIN),
    ponding_models = ponding_models
  )

  next_year_changes <- cbind(
    residential_investment_changes,
    climate_changes %>% dplyr::select(-ageb_id),
    ponding_changes %>% dplyr::select(-ageb_id)
  )
  next_year_study_data <- apply_data_changes(
    this_week_study_data,
    next_year_changes,
    join_columns = PK_JOIN
  )
  megadapt$study_area@data <- next_year_study_data

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
