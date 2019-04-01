PK_JOIN = c("ageb_id"="ageb_id")

create_study_data <- function(study_data) {
  components <-list(flooding_component, climate_component, ponding_component, resident_component, sacmex_component, water_scarcity_index_component)

  for (component in components) {
    study_data <- component$initialize(study_data)
  }

  study_data
}

initial_state <- function(study_data) {
  cbind(
    subset(study_data, select = COLUMNS_TO_SAVE),
    time_sim = rep(0, length(study_data$ageb_id)),
    year_sim = rep(2018, length(study_data$ageb_id)),
    month_sim = rep(12, length(study_data$ageb_id))
  )
}

create_params <-
  function(new_infrastructure_effectiveness_rate = 0.07,
           maintenance_effectiveness_rate = 0.07,
           n_steps = 5,
           infrastructure_decay_rate = 0.01,
           budget = 1200,
           half_sensitivity_ab = 10,
           half_sensitivity_d = 10,
           climate_scenario=1) {
    list(
      new_infrastructure_effectiveness_rate = new_infrastructure_effectiveness_rate,
      maintenance_effectiveness_rate = maintenance_effectiveness_rate,
      n_steps = n_steps,
      infrastructure_decay_rate = infrastructure_decay_rate,
      budget = budget,
      half_sensitivity_ab = half_sensitivity_ab,
      half_sensitivity_d = half_sensitivity_d,
      climate_scenario = climate_scenario
    )
  }

create_megadapt <- function(climate_scenario,
                            mental_models,
                            ponding_models,
                            flooding_models,
                            params,
                            study_area,
                            value_function_config) {
  list(
    climate_scenario = climate_scenario,
    mental_models = mental_models,
    params = params,
    ponding_models = ponding_models,
    flooding_models = flooding_models,
    study_area = study_area,
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
  flooding_models <- megadapt$flooding_models
  study_data <- megadapt$study_area@data %>% dplyr::arrange(ageb_id)
  water_scarcity_model <- megadapt$water_scarcity_model
  value_function_config <- megadapt$value_function_config

  climate_changes <- climate_component$transition(
    study_data = study_data,
    climate_scenario = climate_scenario
  )

  public_infrastructure_changes <- sacmex_component$transition(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    params = params
  )

  water_scarcity_changes <- water_scarcity_index_component$transition(
    study_data = study_data,
    value_function_config=value_function_config
  )

  residential_investment_changes <- resident_component$transition(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    params = params
  )

  ponding_changes <- ponding_component$transition(
    study_data = apply_data_changes(
      study_data,
      climate_changes,
      join_columns = PK_JOIN),
    ponding_models = ponding_models
  )

  flooding_changes <- flooding_component$transition(
    study_data = apply_data_changes(
      study_data,
      climate_changes,
      join_columns = PK_JOIN),
    flooding_models = flooding_models
  )

  sacmex_changes <- sacmex_component$transition(
    study_data = study_data,
    value_function_config = value_function_config,
    mental_models = mental_models,
    params = params
  )

  next_year_changes <- list(
    residential_investment_changes,
    sacmex_changes,
    public_infrastructure_changes,
    water_scarcity_changes,
    climate_changes,
    ponding_changes,
    flooding_changes
  ) %>%
    purrr::reduce(dplyr::left_join, by = PK_JOIN)

  next_year_study_data <- apply_data_changes(
    study_data,
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
    dplyr::summarize(n_weeks = dplyr::n())

  month_counts
}

translate_output_colnames <- function(df) {
  mapping <- c(
    censusblock_id = "ageb_id",
    non_potable_water_infrastructure_age = "antiguedad_dren",
    potable_water_infrastructure_age = "antiguedad_dist",
    days_with_ponding = "encharca",
    days_with_flooding = "inunda",
    stormwater_entrance_count = "rejillas",
    non_potable_water_system_capacity = "q100",
    percent_without_potable_water = "falta_dist",
    days_no_potable_water = "lambdas",
    income_per_capita = "income_pc",
    water_scarcity_index = "scarcity_index",
    potable_water_sensitivity_index = "sensitivity_Ab",
    non_potable_water_sensitivity_index = "sensitivity_D",
    potable_water_vulnerability_index = "vulnerability_Ab",
    non_potable_water_vulnerability_index = "vulnerability_D",
    potable_water_system_intervention_count = "Interventions_Ab",
    non_potable_water_system_intervention_count = "Interventions_D"
  )
  df %>%
    dplyr::rename(!! mapping)
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

  translate_output_colnames(results)
}
