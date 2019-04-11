scenario_index_path <- function(path) {
  fs::path(path, 'index.Rds')
}

scenario_dir <- function(path) {
  fs::path(path, 'results')
}

scenario_path <- function(path, pk) {
  fs::path(scenario_dir(path), glue::glue('{pk}.Rds'))
}

save_scenario_to_cache <- function(data, path, pk) {
  saveRDS(object=data, file=scenario_path(path, pk))
}

delete_scenario_cache <-function(path) {
  fs::dir_delete(path)
}


#' Create a scenario cache by running and saving all experiments in the scenario dataframe
#'
#' @param scenarios data.frame of all experiment combinations to run
#' @param path file path to the scenario cache
#' @param runner callback to run the simulation parameterized by a row of the scenarios data.frame
create_scenario_cache <- function(scenarios, path, runner) {
  if (is.null(scenarios$pk)) {
    scenarios <- scenarios %>%
      dplyr::mutate(pk = row_number())
  }

  fs::dir_create(scenario_dir(path))
  saveRDS(object=scenarios, file=scenario_index_path(path))
  for (scenario_row_id in seq(nrow(scenarios))) {
    scenario <- scenarios[scenario_row_id,]
    pk <- scenario$pk
    args <- dplyr::select(scenario, -pk)
    data <- do.call(runner, args)
    save_scenario_to_cache(data = data, path = path, pk = pk)
  }

  list(
    index = scenarios,
    path = path
  )
}

#' Build a scenario cache for an experiment by taking the cartesian product of all parameter levels
#'
#' @examples
#' build_scenario_cache("../scenarios/budget_experiment", list(budget=6:12*100))
create_cartesian_scenario_cache <- function(model, path, params) {
  scenarios <- do.call(expand.grid, params)
  create_scenario_cache(
    scenarios = scenarios,
    path = path,
    runner = function(...) {
      new_model <- modify_megadapt_model(model = model, ...)
      simulate_megadapt(new_model)
    }
  )
}


#' Load a scenario cache from a file system
load_scenario_cache <- function(path) {
  index <- readRDS(fs::path(path, 'index.Rds'))
  list(
    index = index,
    path = path
  )
}

#' Load a scenario from a cache by some filter criteria
#'
#' @param cache cache to lookup results from
load_scenario <- function(cache, ...) {
  matching <- cache$index %>%
    dplyr::filter(...) %>%
    assertr::verify(nrow(.) == 1)
  readRDS(scenario_path(cache$path, matching$pk))
}

#' Load all scenarios in a matching matching filter criteria
#'
#' @param cache cache to lookup results from
load_scenarios <- function(cache, ...) {
  matching <- cache$index %>%
    dplyr::filter(...) %>%
    mutate(scenario = purrr::map(pk, ~readRDS(scenario_path(cache$path, .x))))
}
