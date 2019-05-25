library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

megadapt <- megadapt_single_coupled_with_action_weights_create(create_params())
new_results <- simulate(megadapt)
