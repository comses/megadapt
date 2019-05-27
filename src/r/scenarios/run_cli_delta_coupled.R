#!/usr/bin/env Rscript

library("argparse")
library("dplyr")
library("DBI")
library("RPostgreSQL")


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



args <- parser$parse_args()

new_infrastructure_effectiveness_rate = as.numeric(args$effectiveness_new_infra)
maintenance_effectiveness_rate = as.numeric(args$effectiveness_maintenance)
n_steps =  as.numeric(args$steps)
infrastructure_decay_rate =  as.numeric(args$infrastructure_decay)
budget =  as.numeric(args$budget)
half_sensitivity_ab = as.numeric(args$half_sensitivity_ab)
half_sensitivity_d = as.numeric(args$half_sensitivity_d)

source('util.R')

library(megadaptr)
library(magrittr)



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
	  mental_models = mental_model_sacmex_coupled_strategies(),
	  ponding_fnss = ponding_delta_method_fnss_create(),
	  flooding_fnss = flooding_delta_method_fnss_create()
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
