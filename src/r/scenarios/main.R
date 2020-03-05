library(megadaptr)
library(magrittr)
source('../scenarios/util.R')

overrides = list(
  sacmex = list(distance_mode = "normalized"),
  mental_models = list(
    config = list(
      potable = list(stage1 = "potable_water_sacmex_unweighted_stage1.csv",
                      stage2 = "potable_water_sacmex_unweighted_stage2.csv"),

      sewer = list (stage1 = "sewer_water_sacmex_unweighted_stage1.csv",
                    stage2 = "sewer_water_sacmex_unweighted_stage2.csv"),
      change_year = 2
    )),
  value_functions = list(function_folder = "base"))
megadapt <- megadaptr:::megadapt_deserialize(
  config = overrides,
  study_area_path = megadaptr:::data_dir('censusblocks', 'megadapt_wgs84_v8.gpkg'),
  year = 2020,
  n_steps = 5
)




result_config <- megadaptr:::megadapt_config_create(overrides)
megadaptr:::config_flatten(result_config)
new_results <- simulate(megadapt)

#write.csv(new_results,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/eventos_extremos/test_sensitivity.csv", row.names = FALSE)
