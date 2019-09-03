drv <- RSQLite::SQLite()
conn <- DBI::dbConnect(drv, 'experiment.db')

describe('a grid experiment', {
  db_config_path <- experiment_dir('db-sqlite.json')
  experiment_config_path <- experiment_dir('grid.json')
  study_area <- data_dir('censusblocks', 'megadapt_wgs84_v5.gpkg')
  cli_root(c('--db-config', db_config_path, 'grid', 'setup', '--experiment-config', experiment_config_path))

  it('should create an experiment table and params table on setup', {
    tables <- DBI::dbListTables(conn)
    expect_true('experiment' %in% tables)
    expect_true('budget_climate_param' %in% tables)
  })

  it('should create a condor submit script', {
    testthat::expect_true(fs::file_exists('budget_climate.sub'))
  })

  it('should allow running of param combinations given in the params table', {
    if (Sys.getenv('R_INTEGRATION_TESTS') == '') {
      skip('skip running model with cli params')
    }
    suppressWarnings(cli_root(c('--db-config', db_config_path, 'grid', 'run', '--experiment', 'budget_climate', '--id', '1', '--study-area', study_area)))
    result_tbl <- dplyr::tbl(conn, 'budget_climate_result')
    years <- result_tbl %>% dplyr::select(year) %>% dplyr::distinct() %>% dplyr::collect() %>% .$year
    testthat::expect_length(years, 6)
  })
})

teardown({
  DBI::dbDisconnect(conn)
  # fs::file_delete('experiment.db')
  # fs::file_delete('budget_climate.sub')
  # fs::dir_delete('budget_climate')
})
