PK_JOIN_EXPR = c("censusblock_id" = "censusblock_id")

#' Import a study area from the command line using OGR
#'
#' @export
#' @param path the path to the study area file
#' @return a study area
study_area_read <- function(path) {
  bits <- unlist(strsplit(path,"/"))
  layer_name <- strsplit(bits[length(bits)], "[.]")[[1]][[1]]
  sdf <- rgdal::readOGR(dsn = path,
                 layer = layer_name,
                 stringsAsFactors = FALSE,
                 verbose = TRUE,
                 integer64 = 'warn.loss')
  df <- sdf@data %>%
    dplyr::rename(
      household_potable_system_lacking_percent=household_potable_system_percent_lacking,
      resident_reports_flooding_per_year=resident_reports_flooding_per_year,
      sewer_system_capacity_max=sewer_system_max_capacity,
      household_sewer_system_lacking_percent = household_sewer_system_percent_lacking,
      resident_potable_water_lacking_count = resident_potable_water_count_lacking,
      precipitation_volume_mean = prec_mean,
      runoff_volume_mean = runoff_mean,
      resident_reports_ponding_count_mean = resident_reports_ponding_per_year,
      resident_reports_flooding_count_mean = resident_reports_flooding_per_year
    )
  sdf@data <- df
  sdf
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
value_function_config_create <- function(function_folder = "base"){
  value_function_deserialize(list(function_folder = function_folder))
}


value_function_deserialize <- function(config) {

  from_fv_dir <- function(...) system.file(fs::path("rawdata", "funciones_valor", "base", ...), package = 'megadaptr', mustWork = TRUE)

  value_function_root_dir <- function(...) system.file(fs::path("rawdata", "funciones_valor", "csvs", ...), package = 'megadaptr', mustWork = TRUE)
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
    load_value_function_json(from_fv_dir("pres_hid.json"))
    #load_value_function_config(value_function_root_dir("fv_presion_hidraulica_escasez_extrema.csv"))
  fv_infrastructure_age <-
    load_value_function_json(from_fv_dir("antiguedad.json"))
  fv_fix_failure_days <-
    load_value_function_json(from_fv_dir("diassagua.json"))
  fv_critical_zones <-
    load_value_function_json(from_fv_dir("zonas_criticas.json"))
  fv_potable_lacking <-
    load_value_function_json(from_fv_dir("falta_dist.json"))
  fv_subsidencia <-
    load_value_function_config(value_function_root_dir("fv_subsidencia.csv"))

  value_function_config <- create_value_function_config(
    sewer_age = fv_antiguedad_drenaje,
    shortage_age = fv_antiguedad_escasez,
    salt_water_quality = fv_calidad_agua_sodio_escasez,
    shortage_failures = fv_falla_escasez,
    hours_of_service_failure = fv_horas_servicio_escasez,
    hydraulic_pressure_failure = fv_presion_hidraulica_escasez,
    infrastructure_age = fv_infrastructure_age,
    fix_failure_days = fv_fix_failure_days,
    critical_zones = fv_critical_zones,
    potable_lacking = fv_potable_lacking,
    subsidence = fv_subsidencia
  )
}

#' Create a parameter set for the megadapt model
#'
#' @export
#' @param new_infrastructure_effectiveness_rate the percent increase in
#' drainage capacity for a census block if a new non potable infrastructure
#' investment is taken. The percent decrease in potable water reports about
#' pipe leakage and infrastructure failure if potable water investment is
#' undertaken in a census block
#' @param maintenance_effectiveness_rate the percent increase in drainage
#' capacity for a census block if non potable infrastructure maintenance is
#' is undertaken. The percent decrease in potable water infrastructure age
#' if potable water infrastructure maintenance is undertaken
#' @param n_steps the number of years to run the model for
#' @param infrastructure_decay_rate the percent decrease in non potable water
#' infrastructure capacity from infrastructure breakdown in a year
#' @param budget the number of census blocks that can be invested in a year.
#' The budget value is identical for potable and non potable infrastructure
#' (if the budget is 200 then it is 200 for potable and 200 for non potable
#' infrastructure)
#' @param resident_action_efficiency_potable sensitivity to fresh water access
#' @param resident_action_efficiency_drainage sensitivity to ponding and flooding
#' @param resilience_threshold A threhold to define the resilient of residents
#' @param climate_scenario the climate scenario id used to lookup the climate
#' scenario
#' @return a parameter list used to configure a megadapt model
params_create <-
  function(new_infrastructure_effectiveness_rate = 0.1,
           maintenance_effectiveness_rate = 0.1,
           n_steps = 5,
           infrastructure_decay_rate = 0.02,
           budget = 1200,
           resident_action_efficiency_potable = 0.5,
           resident_action_efficiency_drainage = 0.5,
           resilience_threshold = 0.3,
           climate_scenario=1,
           distance_mode = "unnormalized") {
    list(
      new_infrastructure_effectiveness_rate = new_infrastructure_effectiveness_rate,
      maintenance_effectiveness_rate = maintenance_effectiveness_rate,
      n_steps = n_steps,
      infrastructure_decay_rate = infrastructure_decay_rate,
      budget = budget,
      resident_action_efficiency_potable = resident_action_efficiency_potable,
      resident_action_efficiency_drainage = resident_action_efficiency_drainage,
      resilience_threshold = resilience_threshold,
      climate_scenario = climate_scenario,
      distance_mode = distance_mode
    )
  }

#' Creates a megadapt class,
#' @param year The initial year of a simulation.
#' @param n_steps The number of year to run the model for.
#' @param study_area A data frame with the spatial unit.
#' @param climate_fnss A climate component.
#' @param flooding_fnss A flooding component.
#' @param ponding_fnss A ponding component.
#' @param resident_fnss A resident component.
#' @param sacmex_fnss A SACMEX component.
#' @param water_scarcity_index_exposure_fnss A water scarcity exposure component.
#' @param water_scarcity_index_sensitivity_fnss A water scarcity sensitivity component.
#' @param sacmex_fnss_creator The function used to construct a sacmex component from its parameters
#' @return An object of the "megadapt_dtss" class
megadapt_dtss_create <- function(
  year,
  n_steps,
  study_area,
  climate_fnss,
  flooding_fnss,
  ponding_fnss,
  resident_fnss,
  sacmex_fnss,
  water_scarcity_index_sensitivity_fnss,
  water_scarcity_index_exposure_fnss,
  sacmex_fnss_creator
) {
  config <- list(
    initial_year = year,
    year = year,
    n_steps = n_steps,
    study_area = study_area,
    climate_fnss = climate_fnss,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss,
    resident_fnss = resident_fnss,
    sacmex_fnss = sacmex_fnss,
    water_scarcity_index_sensitivity_fnss = water_scarcity_index_sensitivity_fnss,
    water_scarcity_index_exposure_fnss = water_scarcity_index_exposure_fnss,
    sacmex_fnss_creator = sacmex_fnss_creator
  )
  prepend_class(config, 'megadapt_dtss')
}

output_dtss.megadapt_dtss <- function(megadapt_dtss) {
  megadapt_dtss
}

transition_dtss.megadapt_dtss <- function(megadapt_dtss) {
  climate_fnss <- megadapt_dtss$climate_fnss
  flooding_fnss <- megadapt_dtss$flooding_fnss
  ponding_fnss <- megadapt_dtss$ponding_fnss
  resident_fnss <- megadapt_dtss$resident_fnss
  sacmex_fnss <- megadapt_dtss$sacmex_fnss
  study_data <- megadapt_dtss$study_area@data
  water_scarcity_index_sensitivity_fnss <- megadapt_dtss$water_scarcity_index_sensitivity_fnss
  water_scarcity_index_exposure_fnss <- megadapt_dtss$water_scarcity_index_exposure_fnss
  year <- megadapt_dtss$year
  step_in_years <- year - megadapt_dtss$initial_year + 1
  climate_changes <- call_fnss(climate_fnss, study_data)
  climate_augmented_data <- apply_data_changes(study_data, climate_changes, join_columns = PK_JOIN_EXPR)

  flooding_changes <- call_fnss(flooding_fnss, climate_augmented_data, year = year)
  ponding_changes <- call_fnss(ponding_fnss, climate_augmented_data)

  resident_changes <- call_fnss(resident_fnss, study_data,step_in_years)
  sacmex_changes <- call_fnss(sacmex_fnss, year, study_data)
  water_scarcity_index_exposure_changes <- call_fnss(water_scarcity_index_exposure_fnss, study_data)
  water_scarcity_index_sensitivity_changes <- call_fnss(water_scarcity_index_sensitivity_fnss, study_data)

  next_year_changes <- list(
    climate_changes,
    flooding_changes,
    ponding_changes,
    resident_changes,
    sacmex_changes,
    water_scarcity_index_sensitivity_changes,
    water_scarcity_index_exposure_changes
  ) %>%
    purrr::reduce(dplyr::left_join, by = PK_JOIN_EXPR)
  new_study_data <- apply_data_changes(study_data, next_year_changes, join_columns = PK_JOIN_EXPR)

  megadapt_dtss$year <- year + 1
  megadapt_dtss$study_area@data <- new_study_data
  megadapt_dtss
}

data_dir <- function(...) {
  system.file(fs::path('rawdata', ...), package = 'megadaptr', mustWork = TRUE)
}

megadapt_initialize <- function(megadapt) {
  study_data <- megadapt$study_area@data
  year <- megadapt$year

  study_data <- climate_initialize(megadapt$climate_fnss, study_data = study_data) %>%
    sacmex_initialize(study_data = .) %>%
    flooding_initialize(megadapt$flooding_fnss, study_data = ., year = year) %>%
    ponding_initialize(megadapt$ponding_fnss, study_data = .) %>%
    resident_initialize(megadapt$resident_fnss, study_data = .) %>%
    water_scarcity_index_sensitivity_initialize(megadapt$water_scarcity_index_sensitivity_fnss, study_data = .) %>%
    water_scarcity_index_exposure_initialize(megadapt$water_scarcity_index_exposure_fnss, study_data = .)

  megadapt$study_area@data <- study_data
  megadapt
}

#' Construct a megadapt model object
#'
#' @export
#' @param params a list of params to initalize model components.
#' @param sacmex_fnss_creator an object to create a sacmex component.
#' @param mental_models A mental model object. If NULL the model assumed a mental model with single coupling
#' @param flooding_fnss A flooding model object. If null, a value funcion method model is created.
#' @param ponding_fnss A ponding model object. If null, a value funcion method model is created.
#' @param study_area A spatial polygon data frame of the study area
#' @return A megadapt model object with classes associated to subcomponent objects. The current components in the megadapt object are: Parameters, value_function_config, study_area, mental model object, climate scenario (climate_fnss), flooding model object, ponding model object, and scarcity index object.
megadapt_create <- function(
  params,
  flooding_fnss = NULL,
  mental_models = NULL,
  ponding_fnss = NULL,
  sacmex_fnss_creator = sacmex_seperate_action_budgets_fnss_create,
  study_area = NULL) {

  assert_shape(
    params,
    shape = list(
      new_infrastructure_effectiveness_rate = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      maintenance_effectiveness_rate = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      n_steps = function(x) checkmate::check_int(x, lower = 0),
      infrastructure_decay_rate = function(x) checkmate::check_numeric(0.01, lower = 0, upper = 1),
      budget = function(x) checkmate::check_int(x, lower = 0),
      resident_action_efficiency_potable = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      resident_action_efficiency_drainage = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      resilience_threshold = function(x) checkmate::check_numeric(0.01, lower = 0, upper = 1),
      climate_scenario = function(x) checkmate::check_int(x, lower = 1, upper = 12)
    ))

  if (is.null(flooding_fnss)) {
    flooding_fnss <- flooding_delta_method_fnss_create()
  }

  if (is.null(ponding_fnss)) {
    ponding_fnss <- ponding_delta_method_fnss_create()
  }

  if (is.character(sacmex_fnss_creator)) {
    checkmate::assert_choice(sacmex_fnss_creator, choices = c('sacmex_seperate_action_budgets_fnss_create', 'sacmex_fnss_create'))
    sacmex_fnss_creator <- get(sacmex_fnss_creator)
  }

  if (is.null(mental_models)) {
    mental_models <- mental_model_constant_strategies()
  }

  if (is.null(study_area)) {
    study_area <- study_area_read(data_dir('censusblocks', 'megadapt_wgs84_v15.gpkg'))
  }

  value_function_config <- value_function_config_default()
  climate_fnss <- climate_fnss_create(params$climate_scenario)
  resident_fnss = resident_fnss_create(
    value_function_config = value_function_config,
    mental_model_strategy = mental_models$resident_limit_strategy,
    resident_action_efficiency_potable = params$resident_action_efficiency_potable,
    resident_action_efficiency_drainage = params$resident_action_efficiency_drainage,
    resilience_threshold = params$resilience_threshold
  )
  sacmex_fnss = sacmex_fnss_creator(
    value_function_config = value_function_config,
    sewer_mental_model_strategy = mental_models$sewer_water_sacmex_limit_strategy,
    potable_water_mental_model_strategy = mental_models$potable_water_sacmex_limit_strategy,
    params = params,
    potable_water_budget = params$budget,
    sewer_budget = params$budget,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss,
    distance_mode = params$distance_mode
  )
  water_scarcity_index_sensitivity_fnss = water_scarcity_index_sensitivity_fnss_create(value_function_config = value_function_config)
  water_scarcity_index_exposure_fnss = water_scarcity_index_exposure_fnss_create(value_function_config = value_function_config)


  megadapt_dtss_create(
    year = 2020,
    n_steps = params$n_steps,
    study_area = study_area,
    climate_fnss = climate_fnss,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss,
    resident_fnss = resident_fnss,
    sacmex_fnss = sacmex_fnss,
    water_scarcity_index_sensitivity_fnss = water_scarcity_index_sensitivity_fnss,
    water_scarcity_index_exposure_fnss = water_scarcity_index_exposure_fnss,
    sacmex_fnss_creator = sacmex_fnss_creator
  )
}

megadapt_config_create <- function(overrides) {
  config <- list()
  subconfigs <- list(
    climate = climate_config_create,
    flooding = flooding_config_create,
    mental_models = mental_model_config_create,
    ponding = ponding_config_create,
    resident = resident_config_create,
    sacmex = sacmex_config_create,
    water_scarcity_exposure = water_scarcity_exposure_config_create,
    water_scarcity_sensitivity = water_scarcity_sensitivity_config_create,
    value_functions = value_function_config_create
  )
  for (subconfig_name in names(subconfigs)) {
    if (is.null(overrides[[subconfig_name]])) {
      override <- list()
    } else {
      override <- overrides[[subconfig_name]]
    }
    config[[subconfig_name]] <- do.call(subconfigs[[subconfig_name]], override)
  }
  config
}

megadapt_deserialize <- function(config, study_area_path, year, n_steps) {
  components <- list()
  keys <- c(
    'climate',
    'flooding',
    'mental_models',
    'ponding',
    'resident',
    'sacmex',
    'water_scarcity_exposure',
    'water_scarcity_sensitivity',
    'value_functions'
  )

  for (key in keys) {
    if (is.null(config[[key]])) {
      config[[key]] <- list()
    }
  }

  value_function_config <- value_function_deserialize(config$value_functions)

  study_area <- study_area_read(study_area_path)

  climate_fnss <- climate_deserialize(config$climate)
  flooding_fnss <- flooding_deserialize(config$flooding)
  ponding_fnss <- ponding_deserialize(config$ponding)
  mental_models <- mental_model_deserialize(config$mental_models)
  resident_fnss <- resident_deserialize(
    config$resident,
    value_function_config = value_function_config,
    mental_model_strategy = mental_models$resident_limit_strategy)
  sacmex_fnss <- sacmex_deserialize(
    config = config$sacmex,
    value_function_config = value_function_config,
    sewer_mental_model_strategy = mental_models$sewer_water_sacmex_limit_strategy,
    potable_water_mental_model_strategy = mental_models$potable_water_sacmex_limit_strategy,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss
  )
  water_scarcity_index_exposure_fnss <- water_scarcity_exposure_deserialize(
    config = config$water_scarcity_exposure,
    value_function_config = value_function_config
  )
  water_scarcity_index_sensitivity_fnss <- water_scarcity_sensitivity_deserialize(
    config = config$water_scarcity_sensitivity,
    value_function_config = value_function_config
  )

  megadapt_dtss_create(
    year = year,
    n_steps = n_steps,
    study_area = study_area,
    climate_fnss = climate_fnss,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss,
    resident_fnss = resident_fnss,
    sacmex_fnss = sacmex_fnss,
    water_scarcity_index_sensitivity_fnss = water_scarcity_index_exposure_fnss,
    water_scarcity_index_exposure_fnss = water_scarcity_index_sensitivity_fnss,
    sacmex_fnss_creator = NULL # do I need this
  )
}

#' Run the megadapt model
#'
#' @export
#' @param megadapt a megadapt model instance
simulate <- function(megadapt) {
  megadapt <- megadapt_initialize(megadapt)
  study_data <- output_dtss(megadapt)$study_area@data
  results <- save_results(study_data = study_data, year = megadapt$year)
  megadapt <- transition_dtss(megadapt)
  for (i in seq(megadapt$n_steps)) {
    study_data <- output_dtss(megadapt)$study_area@data
    results <- save_results(study_data = study_data, result_prev_time = results, year = megadapt$year)
    megadapt <- transition_dtss(megadapt)
  }
  results
}
