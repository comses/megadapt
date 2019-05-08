library(megadaptr)

initialize_model_cache <- function() {
  mental_model_file_names = list(
    potable_water_operator_limit = 'DF101215_GOV_AP modificado PNAS.limit.csv',
    non_potable_water_operator_limit = 'SACMEX_Drenaje_limit_SESMO.csv',
    overall_limit = 'I080316_OTR.limit.csv'
  )

  if (fs::file_exists('/in_docker')) {
    data_root_dir <- '/srv/data'
    cache_path <- '/srv/cache'
  } else {
    data_root_dir <- '../../../../data'
    cache_path <- 'budget_experiment'
  }

  budget <- 1:6 * 400
  megadapt <- build_megadapt_model(data_root_dir = data_root_dir,
                                   mental_model_file_names = mental_model_file_names)
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
