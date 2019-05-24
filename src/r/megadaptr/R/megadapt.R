study_area_read <- function(path) {
  rgdal::readOGR(dsn = path,
                 layer = 'megadapt_wgs84_v3',
                 stringsAsFactors = FALSE,
                 verbose = TRUE,
                 integer64 = 'warn.loss')
}

value_function_config_default <- function() {
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
}


megadapt_dtss_create <- function(
  year,
  study_area,
  climate_fnss,
  flooding_fnss,
  ponding_fnss,
  resident_dtss,
  sacmex_dtss,
  water_scarcity_fnss
) {
  config <- list(
    year = year,
    study_area = study_area,
    climate_fnss = climate_fnss,
    flooding_fnss = flooding_fnss,
    ponding_fnss = ponding_fnss,
    resident_fnss = resident_fnss,
    sacmex_fnss = sacmex_fnss,
    water_scarcity_fnss = water_scarcity_fnss
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
  water_scarcity_fnss <- megadapt_dtss$water_scarcity_fnss
  year <- megadapt_dtss$year

  climate_changes <- call_fnss(climate_fnss, year)
  climate_augmented_data <- apply_data_changes(study_data, climate_changes)
  flooding_changes <- call_fnss(flooding_fnss, climate_augmented_data)
  ponding_changes <- call_fnss(ponding_fnss, climate_augmented_data)

  resident_changes <- call_fnss(resident_fnss, year, study_data)
  sacmex_changes <- call_fnss(sacmex_fnss, year, study_data)
  water_scarcity_index_changes <- call_fnss(water_scarcity_fnss, study_data)

  next_year_changes <- list(
    climate_changes,
    flooding_changes,
    ponding_changes,
    residential_changes,
    sacmex_changes,
    water_scarcity_changes
  ) %>%
    purrr::reduce(dplyr::left_join, by = PK_JOIN)
  new_study_data <- aapply_data_changes(study_data, next_year_changes)

  megadapt_dtss$year <- year + 1
  megadapt_dtss$study_area@data <- new_study_data
  megadapt_dtss
}

megadapt_double_coupled_with_action_weights_create <- function() {
}

megadapt_double_coupled_with_split_budget_create <- function() {

}

data_dir <- function(...) {
  system.file(fs::path('rawdata', ...), package = 'megadaptr', mustWork = TRUE)
}

megadapt_single_coupled_with_action_weights_create <- function(params) {
  value_function_config <- value_function_config_default()
  mental_models <- mental_model_constant_strategies()
  study_area = study_area_read(data_dir('censusblocks', 'megadapt_wgs84_v3.gpkg'))
  climate_fnss <- climate_fnss_create(
    data_dir('climate_landuse_scenarios', 'df_prec_escorrentias_0_ff45.csv'))
  flooding_fnss = flooding_index_fnss_create()
  ponding_fnss = ponding_index_fnss_create()
  resident_fnss = resident_fnss_create()
  sacmex_fnss = sacmex_seperate_action_budgets_fnss_create()
  water_scarcity_fnss = water_scarcity_index_fnss_create(value_function_config)
  megadapt_dtss_create(
    year = params$start_year,
    climate_fnss = climate_fnss
  )
}

megadapt_single_coupled_with_split_budget_create <- function() {

}

simulate <- function(model) {
  study_data <- output_dtss(model)
  results <- save_TS(study_data = study_data, year = model$year)
  model <- transition_dtss(model)
  for (i in seq(model$n_steps)) {
    df <- output_dtss(model)
    results <- save_TS(study_data = study_data, result_prev_time = results, year = model$year)
    model <- transition_dtss(model)
  }
  results
}
