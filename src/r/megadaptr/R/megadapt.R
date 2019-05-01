PK_JOIN = c("ageb_id"="ageb_id")

create_study_data <- function(megadapt) {
  components <-
    list(
      climate_component,
      flooding_component,
      ponding_component,
      resident_component,
      sacmex_component,
      water_scarcity_index_component
    )

  study_data <- megadapt$study_area@data
  args <- megadapt[names(megadapt) != "study_area"]
  for (component in components) {
    args$study_data <- study_data
    arg_names <- rlang::fn_fmls_names(component$initialize)
    study_data <- do.call(component$initialize, args[arg_names])
  }

  megadapt$study_area@data <- study_data
  megadapt
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
    potable_percent_lacking = "falta_dist",
    non_potable_percent_lacking = "falta_dren",
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


#' Build a megadapt model
#'
#' @param data_root_dir The base directory from where to locate all the data and configuration files
#' @param mental_model_file_names File names for the analytic network process limit matrices
#' @param params Parameterization of the megadapt model created from \code{\link{create_params}}
build_megadapt_model <- function(data_root_dir, mental_model_file_names, params = list()) {
  #
  # Param Setup
  #
  params <- do.call(create_params, params)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(data_root_dir)

  #
  # Flooding Model Setup
  #
  flooding_models <- load_flooding_models(data_root_dir)

  #
  # Climate Scenario Setup
  #
  climate_scenario_index = as.data.frame(read.csv(fs::path(data_root_dir, "climate_landuse_scenarios", "index.csv"), header = T))

  scenario_name = climate_scenario_index[which(climate_scenario_index$id == params$climate_scenario), ]$path

  climate_scenario <- read.csv(fs::path(data_root_dir, "climate_landuse_scenarios", scenario_name))

  #
  # Value Function Setup
  #
  value_function_root_dir <- function(...) fs::path(data_root_dir, "funciones_valor", "csvs", ...)
  fv_antiguedad_drenaje <-
    load_value_function_config(value_function_root_dir("fv_antiguedad_drenaje.csv"))
  fv_antiguedad_escasez <-
    load_value_function_config(value_function_root_dir("fv_antiguedad_escasez.csv"))
  fv_calidad_agua_sodio_escasez <-
    load_value_function_config(value_function_root_dir("fv_calidad_agua_sodio_escasez.csv"))
  fv_falla_escasez <-
    load_value_function_config(value_function_root_dir("fv_falla_escasez.csv"))
  fv_horas_servicio_escasez <-
    load_value_function_config(value_function_root_dir("fv_horas_servicio_escasez.csv"))
  fv_presion_hidraulica_escasez <-
    load_value_function_config(value_function_root_dir("fv_presion_hidraulica_escasez.csv"))
  fv_subsidencia <-
    load_value_function_config(value_function_root_dir("fv_subsidencia.csv"))

  value_function_config <- create_value_function_config(
    sewer_age = fv_antiguedad_drenaje,
    shortage_age = fv_antiguedad_escasez,
    salt_water_quality = fv_calidad_agua_sodio_escasez,
    shortage_failures = fv_falla_escasez,
    hours_of_service_failure = fv_horas_servicio_escasez,
    hydraulic_pressure_failure = fv_presion_hidraulica_escasez,
    subsidence = fv_subsidencia
  )

  #
  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      fs::path(data_root_dir, "censusblocks/megadapt_wgs84.shp"),
      verbose = FALSE,
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    )

  #
  # Mental Model Setup
  #
  mm_water_operator_s_lim <-
    data.frame(read.csv(
      fs::path(data_root_dir, "mental_models", mental_model_file_names$potable_water_operator_limit),
      skip = 1,
      header = T
    ))[,-c(1, 2, 21)]
  mm_water_operator_d_lim <-
    data.frame(read.csv(
      fs::path(data_root_dir, "mental_models", mental_model_file_names$non_potable_water_operator_limit),
      skip = 1,
      header = T
    ))[,-c(1, 2)]
  mm_iz <-
    data.frame(read.csv(
      fs::path(data_root_dir, "mental_models", mental_model_file_names$overall_limit),
      skip = 1,
      header = T
    ))[,-c(1, 2)]

  mental_models <- create_mental_models(
    mm_water_operator_d_lim = mm_water_operator_d_lim,
    mm_water_operator_s_lim = mm_water_operator_s_lim,
    mm_iz = mm_iz
  )

  #
  # Build Main Model
  #
  megadapt <- create_megadapt(
    climate_scenario = climate_scenario,
    mental_models = mental_models,
    params = params,
    ponding_models = ponding_models,
    flooding_models = flooding_models,
    study_area = study_area,
    value_function_config = value_function_config
  )
  megadapt <- create_study_data(megadapt)
  megadapt
}

#' Modify an existing megadapt model
#'
#' @param model megadapt model
modify_megadapt_model <- function(model, ...) {
  model$params <- create_params(...)
  model
}
