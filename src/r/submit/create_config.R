create_config <- function(clusterID) {
  SA_conditions <- list(
    exp_min = 1,
    exp_max = 2 ,
    run_model = T,
    on_cluster = F,
    summary_stats = c("mean","max","min")
  )
  SA_params <- list(
    p1 = list(
      name = "new_infrastructure_effectiveness_rate",
      min = 0.01,
      max = 0.3,
      isInteger = F
    ),
    p2 = list(
      name = "maintenance_effectiveness_rate",
      min = 0.1,
      max = 0.3,
      isInteger = F
    ),
    p3 = list(
      name = "infrastructure_decay_rate",
      min = 0.001,
      max = 1,
      isInteger = F
    ),
    p4 = list(
      name = "budget",
      min = 24,
      max = 2428,
      isInteger = T
    ),
    p5 = list(
      name = "climate_scenario",
      min = 1,
      max = 12,
      isInteger = T
    )
  )

  noParams <- 4 #number of parameters to take into account
  SA_params <- SA_params[1:noParams]

  megadapt_conds <- list(
    sim_years = 4,
    municip = T,
    out_stats = c("mean","max","min"),
    out_metric_names = c("household_potable_water_vulnerability", "household_sewer_vulnerability", "flooding_index",
                         "ponding_index","scarcity_index_exposure", "scarcity_index_sensitivity")
  )

  experiment_name <- paste("sa_exponent_",SA_conditions$exp_max,"_id_",clusterID, sep = "")

  description <- paste("Variance Based Sensitivity Analysis on", length(SA_params),"parameters and sample size of", (2^(SA_conditions$exp_max)))

  params_table <- paste("parameters_", experiment_name, sep = "")

  results_table <- paste("results_", experiment_name, sep = "")

  SA_config <- list(SA_conditions = SA_conditions,
                    SA_params = SA_params,
                    megadapt_conds = megadapt_conds,
                    experiment_name = experiment_name,
                    title = experiment_name,
                    description = description,
                    author_name = "Manuela Vanegas Ferro",
                    params_table = params_table,
                    results_table = results_table)

  jsonlite::write_json(SA_config,"config.json")
}

argms <- commandArgs(trailingOnly = TRUE)

create_config(argms[1])
