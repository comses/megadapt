library(fs)

data_dir <- function(...) {
  system.file('rawdata', ..., package='megadaptr')
}

output_dir <- function(...) {
  fs::path('../../../output/', ...)
}

data_root_dir <- data_dir()

potable_water_cluster <- read_cluster_matrix('inst/rawdata/mental_models/potable_water_cluster_sacmex.csv')
resident_cluster <- read_cluster_matrix('inst/rawdata/mental_models/resident_cluster.csv')

mental_model_strategies = list(
  potable_water_sacmex_limit_strategy = file_constant_mental_model_strategy('inst/rawdata/mental_models/potable_water_sacmex_unweighted_stage1.csv', cluster = potable_water_cluster),
  sewer_water_sacmex_limit_strategy = file_constant_mental_model_strategy('inst/rawdata/mental_models/sewer_water_sacmex_unweighted_stage1.csv', cluster = NULL),
  resident_limit_strategy = file_constant_mental_model_strategy('inst/rawdata/mental_models/resident_unweighted.csv', cluster = resident_cluster)
)

