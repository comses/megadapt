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
#' @export
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
#' @export
#' @param model a megadapt model
#' @param path the file path of where you want the cache to be
#' @param params parameter levels to create new megadapt models with.
#' It is used to create of dataframe with all possible combinations of
#' of parameter levels.
#' @return a scenario cache
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
#'
#' @export
#' @param path the file path to the scenario cache
#' @return a scenario cache
load_scenario_cache <- function(path) {
  index <- readRDS(fs::path(path, 'index.Rds'))
  list(
    index = index,
    path = path
  )
}

#' Load a scenario from a cache by some filter criteria
#'
#' @export
#' @param cache cache to lookup results from
#' @param ... filter expressions to apply to the scenario cache.
#' Must resolve to a unique scenario.
#' @return a scenario
load_scenario <- function(cache, ...) {
  matching <- cache$index %>%
    dplyr::filter(...) %>%
    assertr::verify(nrow(.) == 1)
  readRDS(scenario_path(cache$path, matching$pk))
}

#' Load all scenarios in a matching matching filter criteria
#'
#' @export
#' @param cache cache to lookup results from
#' @param ... filter expression to apply
#' @return nested tibble of scenarios matching filter criteria
load_scenarios <- function(cache, ...) {
  matching <- cache$index %>%
    dplyr::filter(...) %>%
    dplyr::mutate(scenario = purrr::map(pk, ~readRDS(scenario_path(cache$path, .x))))
}
