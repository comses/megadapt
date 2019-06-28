library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

megadapt <- megadapt_create(
  create_params(),
  sacmex_fnss_creator = sacmex_fnss_create,
  mental_models = mental_model_constant_strategies(),
  flooding_fnss = flooding_delta_method_fnss_create(),
  ponding_fnss = ponding_delta_method_fnss_create()
)
new_results <- simulate(megadapt)
