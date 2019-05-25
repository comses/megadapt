#!/usr/bin/env Rscript

library("argparse")

parser <- ArgumentParser(description='Run model')

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

# set.seed(1000)

megadapt <- build_megadapt_model(
  data_root_dir = data_root_dir,
  mental_model_file_names = mental_model_file_names,
  params = create_params(
    new_infrastructure_effectiveness_rate = new_infrastructure_effectiveness_rate,
    maintenance_effectiveness_rate = maintenance_effectiveness_rate,
    n_steps = n_steps,
    budget=budget,
    infrastructure_decay_rate = infrastructure_decay_rate,
    half_sensitivity_ab = half_sensitivity_ab,
    half_sensitivity_d = half_sensitivity_d
  )
)

results <- simulate_megadapt(megadapt)

sim_id_output=sprintf("sim_%s_%s_%s_%s_%s_%s_%s.rds",
		      new_infrastructure_effectiveness_rate,
		      maintenance_effectiveness_rate,
		      n_steps,
		      infrastructure_decay_rate,
		      budget,
		      half_sensitivity_ab,
		      half_sensitivity_d)

saveRDS(object=results,
	file = output_dir(sim_id_output))
