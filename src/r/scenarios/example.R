source('../scenarios/util.R')

example <- function(x=numeric(),n_years=5) {
  path_to_source <- "." # change path to use it
  path_td <- "../data/"
  path_to_output <- "../outputs/" # change path to use it

  #Assign values to variables
  custom_params <- list()
  param_ind <- 1
  for (param_name in c('new_infrastructure_effectiveness_rate',
                  'maintenance_effectiveness_rate',
                  'infrastructure_decay_rate',
                  'budget')) {
    if (!is.na(x[param_ind])) {
      custom_params[[param_name]] <- x[param_ind]
    }
    param_ind <- param_ind + 1
  }

  #
  # Param Setup
  #
  params <- do.call(create_params, custom_params)

  #
  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      data_dir("megadapt_wgs84.shp"),#input_layer.shp
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    ) # for flooding model
  # Simulation runs only for the city (CDMX) estado=="09"
  #study_area <- subset(study_area, estado == "09")
  study_area@data <- create_study_data(study_area@data)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(data_dir(""))

  # Flooding Model Setup
  #
  flooding_models <- load_flooding_models(data_dir(""))

  #
  # Water Scarcity Model Setup
  #
  water_security_model <- create_water_scarcity_model(study_area@data)

  #
  # Climate Scenario Setup
  #
  climate_scenario <- read.csv(data_dir("df_prec_esc_85_85.csv"))

  #Table_climate_scenarios=as.data.frame(read.csv("../geosimulation/runoff/db_escenarios_prec_esc_ids.csv",header = T))

  #generate the path to the place where the data frame of the scenario is stored

  #scenario_name=Table_climate_scenarios[which(Table_climate_scenarios$id==scenario),]$path

  #print(scenario_name)

  #to test I added to scenrio 1 the following path
  #"../../data/" which is the same as "path_td" variable
  #read the file and save it inside the run
  #runoff_scenario=read.csv(paste0("../geosimulation/runoff/outputs/", scenario_name),sep=",")


  # Value Function Setup
  #
  fv_antiguedad_drenaje <- load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_drenaje.csv"))
  fv_antiguedad_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_escasez.csv"))
  fv_calidad_agua_sodio_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_calidad_agua_sodio_escasez.csv"))
  fv_falla_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_falla_escasez.csv"))
  fv_horas_servicio_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_horas_servicio_escasez.csv"))
  fv_presion_hidraulica_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_presion_hidraulica_escasez.csv"))
  fv_subsidencia <- load_value_function_config(data_dir("funciones_valor/csvs/fv_subsidencia.csv"))

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

  mm_water_operator_s_lim <- data.frame(read.csv(data_dir("DF101215_GOV_AP modificado PNAS.limit.csv"),
                                                 skip = 1, header = T))[, -c(1, 2, 21)]
  mm_water_operator_d_lim <- data.frame(read.csv(data_dir("SACMEX_Drenaje_limit_SESMO.csv"),
                                                 skip = 1, header = T))[, -c(1, 2)]
  mm_iz <- data.frame(read.csv(data_dir("I080316_OTR.limit.csv"), skip = 1, header = T))[, -c(1, 2)]

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
    water_scarcity_model = water_security_model,
    value_function_config = value_function_config
  )
  megadapt
}
