#' @importFrom DBI dbWriteTable
NULL

experiment_dir <- function(...) {
  system.file(fs::path('experiment', ...), package = 'megadaptr', mustWork = TRUE)
}

experiment_table_append <-
  function(conn, name, title, description, author_name) {
    datetime_created <- lubridate::now()
    df <-
      data.frame(
        name = name,
        title = title,
        description = description,
        author_name = author_name,
        datetime_created = datetime_created
      )
    dbWriteTable(
      conn = conn,
      name = 'experiment',
      value = df,
      append = TRUE,
      row.names = FALSE
    )
  }

config_flatten <- function(config) {
  flatten <- function(config) {
    if (!is.list(config)) list(config)
    else unlist(c(lapply(config, flatten)), recursive = FALSE)
  }
  flattened <- flatten(config)
  result <- list()
  names(flattened) <- purrr::map_chr(names(flattened), function(name) stringr::str_replace_all(name, '\\.', '__'))
  flattened
}

config_unflatten <- function(flattened, path = character()) {
  build_list <- function(path, value) {
    n <- length(path)
    r <- list()
    r[[path[n]]] <- value
    for (p in rev(path[-n])) {
      new_r <- list()
      new_r[[p]] <- r
      r <- new_r
    }
    r
  }

  result <- list()
  for (key in names(flattened)) {
    path <- stringr::str_split(key, '__')[[1]]
    path_list <- build_list(path, flattened[[key]])
    result <- modifyList(result, path_list)
  }
  result
}

params_cartesian_create <- function(flattened) {
  df <- do.call(expand.grid, flattened)
  df$id <- 1:nrow(df)
  df
}

params_table_create <- function(conn, experiment_name, df) {
  dbWriteTable(
    conn = conn,
    name = glue::glue('{experiment_name}_param'),
    value = df,
    row.names = FALSE
  )
}

params_run <- function(conn, experiment_name, id, study_area_path) {
  params_tbl <- dplyr::tbl(conn, glue::glue('{experiment_name}_param'))
  params_df <- params_tbl %>% dplyr::filter(id == !! id) %>% dplyr::collect()
  params <- as.list(params_df %>% dplyr::select(-id, -rep))
  config <- config_unflatten(params)
  model <- megadapt_deserialize(
    config,
    study_area_path = study_area_path,
    year = config$year,
    n_steps = config$n_steps)
  results <- simulate(model)
  results$param_id <- params_df$id
  dbWriteTable(
    conn = conn,
    name = glue::glue('{experiment_name}_result'),
    value = results,
    append = TRUE,
    row.names = FALSE
  )
}

result_condor_submit_create <- function(executable, experiment_name, params_tbl, study_area_path, db_config) {
  tmpl_path <- system.file(fs::path('hpc', 'condor'), package = 'megadaptr', mustWork = TRUE)
  param_ids <- params_tbl %>% dplyr::select(id) %>% dplyr::collect() %>% .$id
  R.rsp::rfile(
    file = 'run.sub.rsp',
    path = tmpl_path,
    output = glue::glue('{experiment_name}.sub'),
    args = list(
      executable = executable,
      db_config = db_config,
      experiment = experiment_name,
      param_ids = param_ids,
      study_area = study_area_path))
  fs::dir_create(experiment_name)
}

results_table_create <- function(conn, name, param_df) {
  for (row_ind in seq(nrow(param_df))) {
    params <-
      do.call(params_create, param_df %>% dplyr::select(-id) %>% .[row_ind, ])
    model <- megadapt_create(params = params)
    results <- simulate(model)
    results$param_id <- param_df %>% .[row_ind, ] %>% .$id
    dbWriteTable(
      conn = conn,
      name = glue::glue('{name}_result'),
      value = results,
      append = TRUE
    )
  }
}

experiment_map_df <- function(results, params, facets) {
  intervention_cols <- list(
    "sacmex_potable_maintenance_intervention_presence",
    "sacmex_sewer_maintenance_intervention_presence",
    "sacmex_potable_new_infrastructure_intervention_presence",
    "sacmex_sewer_new_infrastructure_intervention_presence"
  )

  intervention_labels <- tibble::tribble(
    ~ name,
    ~ label,
    'potable_maintenance_count',
    'Potable Maintenance',
    'potable_new_infra_count',
    'Potable New Infra',
    'sewer_maintenance_count',
    'Sewer Maintenance',
    'sewer_new_infra_count',
    'Sewer New Infra'
  )

  results %>%
    dplyr::select(censusblock_id, param_id,!!!intervention_cols) %>%
    dplyr::inner_join(params, by = c('param_id' = 'id')) %>%
    dplyr::group_by(censusblock_id,!!!facets) %>%
    dplyr::summarise(
      potable_maintenance_count = sum(sacmex_potable_maintenance_intervention_presence),
      sewer_maintenance_count = sum(sacmex_sewer_maintenance_intervention_presence),
      potable_new_infra_count = sum(sacmex_potable_new_infrastructure_intervention_presence),
      sewer_new_infra_count = sum(sacmex_sewer_new_infrastructure_intervention_presence)
    ) %>%
    dplyr::collect() %>%
    tidyr::gather(
      key = statistic_name,
      value = statistic_value,
      potable_maintenance_count,
      potable_new_infra_count,
      sewer_maintenance_count,
      sewer_new_infra_count
    ) %>%
    dplyr::mutate(statistic_name = dplyr::recode(
      statistic_name,!!!(
        intervention_labels %>% tidyr::spread(name, label) %>% as.list
      )
    ))
}

experiment_map_plot_household_investment <- function() {

}
