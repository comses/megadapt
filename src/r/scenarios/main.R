library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

megadapt <- build_megadapt_model(data_root_dir = data_root_dir,
                                 mental_model_strategies = mental_model_strategies)
new_results <- simulate_megadapt(megadapt)
