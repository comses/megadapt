example <- function() {
  path_to_source <- "." # change path to use it
  path_td <- "../data/"
  path_to_output <- "../outputs/" # change path to use it

  #
  # Param Setup
  #
  params <- create_params()

  #
  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      paste(path_td, "Layer_MEGADAPT_Oct2018.shp", sep = ""),
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    ) # for flooding model
  # Simulation runs only for the city (CDMX) estado=="09"
  study_area <- subset(study_area, estado == "09")
  study_area@data <- create_study_data(study_area@data)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(path_td)

  #
  # Water Scarcity Model Setup
  #
  water_security_model <- create_water_scarcity_model(study_area@data)

  #
  # Climate Scenario Setup
  #
  climate_scenario <- read.csv(paste(path_td, "df_prec_esc_85_85.csv", sep = ""))

  #
  # Value Function Setup
  #
  fv_antiguedad_drenaje <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_antiguedad_drenaje.csv", sep = ""))
  fv_antiguedad_escasez <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_antiguedad_escasez.csv", sep = ""))
  fv_calidad_agua_sodio_escasez <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_calidad_agua_sodio_escasez.csv", sep = ""))
  fv_falla_escasez <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_falla_escasez.csv", sep = ""))
  fv_horas_servicio_escasez <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_horas_servicio_escasez.csv", sep = ""))
  fv_presion_hidraulica_escasez <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_presion_hidraulica_escasez.csv", sep = ""))
  fv_subsidencia <- load_value_function_config(paste(path_td, "funciones_valor/csvs/fv_subsidencia.csv", sep = ""))

  value_function_config <- create_value_function_config(
    sewer_age=fv_antiguedad_drenaje,
    shortage_age=fv_antiguedad_escasez,
    salt_water_quality=fv_calidad_agua_sodio_escasez,
    shortage_failures=fv_falla_escasez,
    hours_of_service_failure=fv_horas_servicio_escasez,
    hydraulic_pressure_failure=fv_presion_hidraulica_escasez,
    subsidence=fv_subsidencia
  )

  #
  # Mental Model Setup
  #
  mm_water_operator_s_lim <- data.frame(read.csv(paste(path_td, "DF101215_GOV_AP modificado PNAS.limit.csv", sep = ""),
                                                 skip = 1, header = T))[, -c(1, 2, 21)]
  mm_water_operator_d_lim <- data.frame(read.csv(paste(path_td, "SACMEX_Drenaje_limit_SESMO.csv", sep = ""),
                                                 skip = 1, header = T))[, -c(1, 2)]
  mm_iz <- data.frame(read.csv(paste(path_td, "I080316_OTR.limit.csv", sep = ""), skip = 1, header = T))[, -c(1, 2)]

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
    study_area = study_area,
    water_scarcity_model = water_security_model,
    value_function_config = value_function_config
  )
  megadapt
}
