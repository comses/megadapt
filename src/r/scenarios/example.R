source('../scenarios/util.R')

build_megadapt_model <- function(...) {
  path_to_source <- "." # change path to use it
  path_td <- "../data/"
  path_to_output <- "../outputs/" # change path to use it

  #
  # Param Setup
  #
  params <- create_params(...)

  #
  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      data_dir("censusblocks/megadapt_wgs84.shp"),
      #input_layer.shp
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    ) # for flooding model
  study_area@data <- create_study_data(study_area@data)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(data_dir(""))

  # Flooding Model Setup
  #
  flooding_models <- load_flooding_models(data_dir(""))

  #
  Table_climate_scenarios = as.data.frame(read.csv(
    data_dir(
      "climate_landuse_scenarios/db_escenarios_prec_esc_ids.csv"
    ),
    header = T
  ))

  #generate the path to the place where the data frame of the scenario is stored

  scenario_name = Table_climate_scenarios[which(Table_climate_scenarios$id ==
                                                  params$climate_scenario), ]$path

  # Climate Scenario Setup
  #
  climate_scenario <-
    read.csv(data_dir(paste0(
      "climate_landuse_scenarios/", scenario_name
    )))

  # Value Function Setup
  #
  fv_antiguedad_drenaje <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_drenaje.csv"))
  fv_antiguedad_escasez <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_escasez.csv"))
  fv_calidad_agua_sodio_escasez <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_calidad_agua_sodio_escasez.csv"))
  fv_falla_escasez <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_falla_escasez.csv"))
  fv_horas_servicio_escasez <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_horas_servicio_escasez.csv"))
  fv_presion_hidraulica_escasez <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_presion_hidraulica_escasez.csv"))
  fv_subsidencia <-
    load_value_function_config(data_dir("funciones_valor/csvs/fv_subsidencia.csv"))

  value_function_config <- create_value_function_config(
    sewer_age = fv_antiguedad_drenaje,
    shortage_age = fv_antiguedad_escasez,
    salt_water_quality = fv_calidad_agua_sodio_escasez,
    shortage_failures = fv_falla_escasez,
    hours_of_service_failure = fv_horas_servicio_escasez,
    hydraulic_pressure_failure = fv_presion_hidraulica_escasez,
    subsidence = fv_subsidencia
  )

  # Mental Model Setup
  #
  mm_water_operator_s_lim <-
    data.frame(read.csv(
      data_dir("/mental_models/DF101215_GOV_AP modificado PNAS.limit.csv"),
      skip = 1,
      header = T
    ))[,-c(1, 2, 21)]
  mm_water_operator_d_lim <-
    data.frame(read.csv(
      data_dir("/mental_models/SACMEX_Drenaje_limit_SESMO.csv"),
      skip = 1,
      header = T
    ))[,-c(1, 2)]
  mm_iz <-
    data.frame(read.csv(
      data_dir("/mental_models/I080316_OTR.limit.csv"),
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
  megadapt
}

modify_megdapt_model <- function(model, ...) {
  model$params <- create_params(...)
  model
}

#' Build a scenario cache for an experiment
#'
#' @examples
#' build_scenario_cache("../scenarios/budget_experiment", list(budget=6:12*100))
build_scenario_cache <- function(path, params) {
  model <- build_megadapt_model()
  scenarios <- do.call(expand.grid, params)
  create_scenario_cache(
    scenarios = scenarios,
    path = path,
    runner = function(...) {
      new_model <- modify_megdapt_model(model = model, ...)
      simulate_megadapt(new_model)
    }
  )
}
