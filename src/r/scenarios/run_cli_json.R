#!/usr/bin/env Rscript

library("dplyr")
library("DBI")
library("RPostgreSQL")
source('util.R')

library(megadaptr)
library(magrittr)

fh <- file('stdin')
cfg <- jsonlite::fromJSON(fh)

mental_model_env <- new.env(parent = emptyenv())
mental_model_env$mental_model_constant <-
  mental_model_constant_strategies
mental_model_env$mental_model_coupled <-
  mental_model_sacmex_coupled_strategies

ponding_model_env <- new.env(parent = emptyenv())
ponding_model_env$delta <- ponding_delta_method_fnss_create

flooding_model_env <- new.env(parent = emptyenv())
flooding_model_env$delta <- flooding_delta_method_fnss_create

budget_model_env <- new.env(parent = emptyenv())
budget_model_env$split <- sacmex_seperate_action_budgets_fnss_create
budget_model_env$alphas <- sacmex_fnss_create


new_infrastructure_effectiveness_rate = as.numeric(cfg$effectiveness_new_infra)
maintenance_effectiveness_rate = as.numeric(cfg$effectiveness_maintenance)
n_steps =  as.numeric(cfg$steps)
infrastructure_decay_rate =  as.numeric(cfg$infrastructure_decay)
budget =  as.numeric(cfg$budget)
resident_action_efficiency_potable = as.numeric(cfg$resident_action_efficiency_potable)
resident_action_efficiency_drainage = as.numeric(cfg$resident_action_efficiency_drainage)


megadapt <- megadapt_create(
  list(
    new_infrastructure_effectiveness_rate = cfg$effectiveness_new_infra,
    maintenance_effectiveness_rate = cfg$effectiveness_maintenance,
    n_steps = cfg$steps,
    infrastructure_decay_rate = cfg$infrastructure_decay,
    budget = cfg$budget,
    resident_action_efficiency_potable = cfg$resident_action_efficiency_potable,
    resident_action_efficiency_drainage = cfg$resident_action_efficiency_drainage,
    resilience_threshold = cfg$resilience_threshold,
    climate_scenario = cfg$climate_scenario
  ),
  mental_models = get(cfg$mental_models, envir = mental_model_env)(),
  ponding_fnss = get(cfg$ponding_model, envir = ponding_model_env)(),
  flooding_fnss = get(cfg$flooding_model, envir = flooding_model_env)(),
  sacmex_fnss_creator = get(cfg$budget_model, envir = budget_model_env)
)

results <- simulate(megadapt)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(
  drv,
  dbname = "megadapt",
  host = "tawa",
  port = 5432,
  user = "fidel"
)

params <- data.frame(
  experiment = cfg$experiment,
  effectiveness_new_infra = cfg$effectiveness_new_infra,
  effectiveness_maintenance = cfg$effectiveness_maintenance,
  steps = cfg$steps,
  infrastructure_decay = cfg$infrastructure_decay,
  budget = cfg$budget,
  resident_action_efficiency_drainage = cfg$resident_action_efficiency_drainage,
  resident_action_efficiency_potable = cfg$resident_action_efficiency_potable,
  climate_scenario = cfg$climate_scenario,
  rep = cfg$rep,
  key = cfg$key
)
results <- results %>% mutate(param_id = cfg$key)

dbWriteTable(
  conn = conn,
  name = paste0("params_", cfg$experiment),
  value = params,
  row.names = FALSE,
  append = TRUE
)
dbWriteTable(
  conn = conn,
  name = paste0("results_", cfg$experiment),
  value = results,
  row.names = FALSE,
  append = TRUE
)
