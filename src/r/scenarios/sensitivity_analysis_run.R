library(megadaptr)
source('../scenarios/util.R')

################################################################################################
################################################################################################

megadapt_conds <- list(
  sim_years = 40,
  municip = T,
  out_stats = c("mean","max","min"),
  out_metric_names = c("household_potable_water_vulnerability", "household_sewer_vulnerability", "flooding_index",
                   "ponding_index","scarcity_index_exposure", "scarcity_index_sensitivity")
)

# SA Conditions
SA_conditions <- list(
  exp_min = 1,
  exp_max = 2 ,
  run_model = T,
  on_cluster = F,
  summary_stats = c("mean","max","min")
)

batchtools_resources <- list(nnodes = 1,
                             ntasks = 1,
                             ncpus = 1,
                             walltime = 3600,
                             memory = 2000)

# Input Parameters and their Names

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

book_SA_params <- list(
  p1 = list(
    name = "x1",
    min = 0,
    max = 1,
    isInteger = F
  ),
  p2 = list(
    name = "x2",
    min = 0,
    max = 1,
    isInteger = F
  ),
  p3 = list(
    name = "x3",
    min = 0,
    max = 1,
    isInteger = F
  )
)

noParams <- 4 #number of parameters to take into account
SA_params <- SA_params[1:noParams]


################################################################################################
################################################################################################

print(SA_conditions)

model_f <- megadaptr:::megadapt_superficial_params_simulator(megadapt_conds, SA_params)

# model_f <- megadaptr:::book_example_simulator()

resultsss <- megadaptr:::VBSA(model_f, SA_conditions, SA_params, batchtools_resources)

saveRDS(resultsss, "SAresults")






