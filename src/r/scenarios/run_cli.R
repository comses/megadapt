#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

new_infrastructure_effectiveness_rate = as.numeric(args[1])
maintenance_effectiveness_rate = as.numeric(args[2])
n_steps = as.numeric(args[3])
infrastructure_decay_rate = as.numeric(args[4])
budget = as.numeric(args[5])
half_sensitivity_ab = as.numeric(args[6])
half_sensitivity_d = as.numeric(args[7])

source('../scenarios/create_sim.R')

library(megadaptr)
library(magrittr)

# set.seed(1000)

megadapt <- create_sim(new_infrastructure_effectiveness_rate,
                    maintenance_effectiveness_rate,
                    n_steps,
                    infrastructure_decay_rate,
                    budget,
                    half_sensitivity_ab,
                    half_sensitivity_d)
results <- simulate_megadapt(megadapt)

sim_id_output=sprintf("sim_%s_%s_%s_%s_%s_%s_%s.rds",
                      new_infrastructure_effectiveness_rate,
                      maintenance_effectiveness_rate,
                      n_steps,
                      infrastructure_decay_rate,
                      budget,
                      half_sensitivity_ab,
                      half_sensitivity_d)
  
path_to_output<-"outputs/"

saveRDS(object=results,
        file = paste(path_to_output, sim_id_output, sep=""))
