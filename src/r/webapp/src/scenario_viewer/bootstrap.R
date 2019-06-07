library(megadaptr)

initialize_model_cache <- function() {


  if (fs::file_exists('/in_docker')) {
    data_root_dir <- '/srv/data'
    cache_path <- '/srv/cache'
  } else {
    data_root_dir <- system.file('rawdata', package='megadaptr')
    cache_path <- 'budget_experiment'
  }

  budget <- 1:6 * 400
  megadapt <- build_megadapt_model(data_root_dir = data_root_dir,
                                   mental_model_strategies = megadaptr:::create_constant_mental_model_strategies())
  if (fs::dir_exists(cache_path)) {
    cache <- load_scenario_cache(cache_path)
  } else {
    cache <- create_cartesian_scenario_cache(
      model = megadapt,
      path = cache_path,
      params = list(budget = budget)
    )
  }
  list(megadapt = megadapt,
       cache = cache,
       budget = budget)
}

model_cache <- initialize_model_cache()
