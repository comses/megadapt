library(fs)

data_dir <- function(...) {
  system.file('rawdata', ..., package='megadaptr')
}

output_dir <- function(...) {
  fs::path('../../../output/', ...)
}

data_root_dir <- data_dir()

<<<<<<< Updated upstream
mental_model_strategies <- megadaptr:::create_constant_mental_model_strategies()
=======
mm_file_path <- function(path) system.file(fs::path('rawdata', 'mental_models', path), package = 'megadaptr', mustWork = TRUE)

potable_water_sacmex_cluster <- read_cluster_matrix(mm_file_path('potable_water_cluster_sacmex.csv'))
potable_water_sacmex_limit_strategy <- file_constant_mental_model_strategy(
				    path = mm_file_path('potable_water_sacmex_unweighted_stage1.csv'),
				    cluster = potable_water_sacmex_cluster)

sewer_water_sacmex_limit_strategy <- suppressWarnings(file_constant_mental_model_strategy(
				      path = mm_file_path('sewer_water_sacmex_unweighted_stage1.csv'),
				      cluster = NULL))

resident_cluster <- read_cluster_matrix(mm_file_path('resident_cluster.csv'))
resident_limit_strategy <- file_constant_mental_model_strategy(
			       path = mm_file_path('resident_unweighted.csv'),
			       cluster = resident_cluster)
>>>>>>> Stashed changes



mental_model_strategies = list(
 potable_water_sacmex_limit_strategy = potable_water_sacmex_limit_strategy,
 sewer_water_sacmex_limit_strategy = sewer_water_sacmex_limit_strategy,
 resident_limit_strategy = resident_limit_strategy
 )
