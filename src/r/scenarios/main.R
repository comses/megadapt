library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

megadapt <- megadaptr:::megadapt_deserialize(
  config = list(),
  study_area_path = megadaptr:::data_dir('censusblocks', 'megadapt_wgs84_v7.gpkg'),
  layer_name = 'megadapt_wgs84_v7',
  year = 2020,
  n_steps = 5)
new_results <- simulate(megadapt)
