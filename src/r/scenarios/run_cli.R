#!/usr/bin/env Rscript

library("argparse")
library("dplyr")
library("DBI")
library("RPostgreSQL")
source('util.R')

library(megadaptr)
library(magrittr)


parser <- ArgumentParser(description='Run model')

parser$add_argument("--experiment",
		    type="character", required=T,
		    help="name of experiment, db tables will be created with this name")
parser$add_argument("--effectiveness_new_infra",
		    type="double", help="rate of effectiveness of new infrastructure", required=T)
parser$add_argument("--effectiveness_maintenance",
		    type="double", help="rate of effectiveness of maintenance", required=T)
parser$add_argument("--steps",
		    type="integer", help="run this many steps", default=10)
parser$add_argument("--infrastructure_decay",
		    type="double", help="rate of infrastructure decay", required=T)
parser$add_argument("--budget",
		    type="double", help="annual budget for water authority", required=T)
parser$add_argument("--half_sensitivity_d",
		    type="double", required=T, help="?")
parser$add_argument("--half_sensitivity_ab",
		    type="double", required=T, help="?")
parser$add_argument("--climate_scenario",
		    type="integer", required=T, help="index of climate scenario <1..12>")
parser$add_argument("--rep",
		    type="integer", required=T, help="number of experiment repetition")
parser$add_argument("--key",
		    type="integer", required=T, help="key joins params table to results table")
parser$add_argument("--mental_models",
                    type="character", required=T,
                    help="name of the mental model strategy constructor")
parser$add_argument("--ponding_model",
                    type="character", required=T,
                    help="name of the ponding model")
parser$add_argument("--flooding_model",
                    type="character", required=T,
                    help="name of the flooding model")
parser$add_argument("--budget_model",
                    type="character", required=T,
                    help="name of the strategy to allocate the budget")





args <- parser$parse_args()



mental_model_env <- new.env(parent = emptyenv())
mental_model_env$mental_model_constant <- mental_model_constant_strategies
mental_model_env$mental_model_coupled <- mental_model_sacmex_coupled_strategies

ponding_model_env <- new.env(parent = emptyenv())
ponding_model_env$delta <- ponding_delta_method_fnss_create
ponding_model_env$multicriteria <- ponding_index_fnss_create

flooding_model_env <- new.env(parent = emptyenv())
flooding_model_env$delta <- flooding_delta_method_fnss_create
flooding_model_env$multicriteria <- flooding_index_fnss_create

budget_model_env <- new.env(parent = emptyenv())
budget_model_env$split <- sacmex_seperate_action_budgets_fnss_create
budget_model_env$alphas <- sacmex_fnss_create


new_infrastructure_effectiveness_rate = as.numeric(args$effectiveness_new_infra)
maintenance_effectiveness_rate = as.numeric(args$effectiveness_maintenance)
n_steps =  as.numeric(args$steps)
infrastructure_decay_rate =  as.numeric(args$infrastructure_decay)
budget =  as.numeric(args$budget)
half_sensitivity_ab = as.numeric(args$half_sensitivity_ab)
half_sensitivity_d = as.numeric(args$half_sensitivity_d)





    megadapt <- megadapt_single_coupled_with_action_weights_create(
	list(
	    new_infrastructure_effectiveness_rate = args$effectiveness_new_infra,
	    maintenance_effectiveness_rate = args$effectiveness_maintenance,
	    n_steps = args$steps,
	    infrastructure_decay_rate = args$infrastructure_decay,
	    budget = args$budget,
	    half_sensitivity_ab = args$half_sensitivity_ab,
	    half_sensitivity_d = args$half_sensitivity_d,
	    start_year = lubridate::ymd('2019-01-01'),
	    climate_scenario = args$climate_scenario
	    ),
	  mental_models = get(args$mental_models, envir = mental_model_env)(),
	  ponding_fnss = get(args$ponding_model, envir = ponding_model_env)(),
	  flooding_fnss = get(args$flooding_model, envir = flooding_model_env)(),
	  sacmex_fnss_creator = get(args$budget_model, envir = budget_model_env)
)

results <- simulate(megadapt)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
		 host = "tawa", port = 5432,
		 user = "fidel")

params <- data.frame(experiment=args$experiment,
		     effectiveness_new_infra=args$effectiveness_new_infra,
		     effectiveness_maintenance=args$effectiveness_maintenance,
		     steps=args$steps,
		     infrastructure_decay=args$infrastructure_decay,
		     budget=args$budget,
		     half_sensitivity_d=args$half_sensitivity_d,
		     half_sensitivity_ab=args$half_sensitivity_ab,
		     climate_scenario=args$climate_scenario,
		     rep=args$rep,
		     key=args$key)
results <- results %>% mutate(param_id = args$key)

dbWriteTable(conn=conn, name=paste0("params_", args$experiment), value=params, row.names=FALSE, append=TRUE)
dbWriteTable(conn=conn, name=paste0("results_", args$experiment), value=results, row.names=FALSE, append=TRUE)
