#' @importFrom DBI dbWriteTable
NULL

params_cartesian_create <- function(param_levels) {
  df <- do.call(expand.grid, param_levels)
  df$id <- 1:nrow(df)
  df
}

params_table_create <- function(conn, name, df) {
  dbWriteTable(conn = conn, name = glue::glue('{name}_params'), value = df)
}

results_table_create <- function(conn, name, param_df) {
  for (row_ind in seq(nrow(param_df))) {
    params <- do.call(params_create, param_df %>% dplyr::select(-id) %>% .[row_ind,])
    model <- megadapt_create(params = params)
    results <- simulate(model)
    results$param_id <- param_df %>% .[row_ind,] %>% .$id
    dbWriteTable(conn = conn, name = glue::glue('{name}_results'), value = results, append = TRUE)
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
    ~name, ~label,
    'potable_maintenance_count', 'Potable Maintenance',
    'potable_new_infra_count', 'Potable New Infra',
    'sewer_maintenance_count', 'Sewer Maintenance',
    'sewer_new_infra_count', 'Sewer New Infra'
  )

  results %>%
    dplyr::select(censusblock_id, param_id, !!! intervention_cols) %>%
    dplyr::inner_join(params, by = c('param_id'='id')) %>%
    dplyr::group_by(censusblock_id, !!! facets) %>%
    dplyr::summarise(
      potable_maintenance_count = sum(sacmex_potable_maintenance_intervention_presence),
      sewer_maintenance_count = sum(sacmex_sewer_maintenance_intervention_presence),
      potable_new_infra_count = sum(sacmex_potable_new_infrastructure_intervention_presence),
      sewer_new_infra_count = sum(sacmex_sewer_new_infrastructure_intervention_presence)
    ) %>%
    dplyr::collect() %>%
    tidyr::gather(key=statistic_name, value=statistic_value,
                  potable_maintenance_count, potable_new_infra_count,
                  sewer_maintenance_count, sewer_new_infra_count) %>%
    dplyr::mutate(statistic_name = dplyr::recode(statistic_name,
                                                 !!! (intervention_labels %>% tidyr::spread(name, label) %>% as.list)))
}

experiment_map_plot_household_investment <- function() {

}
