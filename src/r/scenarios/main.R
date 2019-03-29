source('../scenarios/example.R')

library(megadaptr)
library(magrittr)

# set.seed(1000)

megadapt <- example()

new_results <- simulate_megadapt(megadapt)

map_results(megadapt,new_results)
