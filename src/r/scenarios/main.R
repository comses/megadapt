library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

megadapt <- megadaptr:::megadapt_deserialize(
  config = list(sacmex = list(distance_mode = "normalized")),
  study_area_path = megadaptr:::data_dir('censusblocks', 'megadapt_wgs84_v8.gpkg'),
  year = 2020,
  n_steps = 5
  )
new_results <- simulate(megadapt)

#write.csv(new_results,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/eventos_extremos/test_sensitivity.csv", row.names = FALSE)
