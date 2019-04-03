source('../scenarios/example.R')

library(megadaptr)
library(magrittr)

# set.seed(1000)

megadapt <- build_megadapt_model()

new_results <- simulate_megadapt(megadapt)
