cli_create <- function() {
  root <-
    argparse::ArgumentParser(description = 'Run the megadapt model')
  root$add_argument('--db-config', help = 'JSON config file used to connect to db', default='db.json')

  subparser <-
    root$add_subparsers(help = 'Subcommands', dest = 'command')

  run <- subparser$add_parser('run', help = 'Run the Megadapt model')
  run$add_argument('--experiment', help = 'Name of the experiment', required=TRUE)
  run$add_argument('--id', help = 'Row ID of the params combination', required=TRUE)
  run$add_argument('--study-area', help = 'Path to study area data', default=data_dir('censusblocks', 'megadapt_wgs84_v5.gpkg'))

  grid_root <- subparser$add_parser('grid', help = 'Grid Search')
  grid_subparser <- grid_root$add_subparsers(help = 'Grid Search subcommands', dest = 'grid_command')

  grid_setup <- grid_subparser$add_parser('setup', help = 'Setup a Grid Search experiment')
  grid_setup$add_argument('--experiment-config', help = 'JSON file describing the model setup', required=TRUE)
  grid_setup$add_argument('--study-area', help = 'Path to study area data', default=data_dir('censusblocks', 'megadapt_wgs84_v5.gpkg'))

  vbsa_root <-
    subparser$add_parser('vbsa', help = 'Variable Based Sensitivity Analysis')
  vbsa_subparser <-
    vbsa_root$add_subparsers(help = 'VBSA subcommands', dest = 'vbsa_command')

  vbsa_setup <-
    vbsa_subparser$add_parser('setup', help = 'Setup a VBSA experiment')
  vbsa_setup$add_argument('--experiment-config', help = 'JSON file describing the model setup', required=TRUE)

  vbsa_reduce <-
    vbsa_subparser$add_parser('reduce', help = 'Reduce the Megadapt model runs and return a sensitivity analysis report')
  vbsa_reduce$add_argument('--experiment', help = 'Name of the experiment to collect results from', required=TRUE)

  root
}

cli_db_connect <- function(db_config_path) {
  if (!fs::file_exists(db_config_path)) {
    stop('DB connection config path must exist')
  }
  fh <- file(db_config_path)
  db_config <- jsonlite::fromJSON(fh)
  switch(db_config$driver,
         postgres = cli_db_connect_postgres(
           name = db_config$name,
           host = db_config$host,
           port = db_config$port,
           user = db_config$user),
         sqlite = cli_db_connect_sqlite(db_config$file))
}

cli_db_connect_postgres <- function(name, host, port, user) {
  drv <- RPostgreSQL::PostgreSQL()
  conn <- DBI::dbConnect(
    drv,
    dbname = name,
    host = host,
    port = port,
    user = user
  )
  conn
}

cli_db_connect_sqlite <- function(name) {
  drv <- RSQLite::SQLite()
  conn <- DBI::dbConnect(
    drv,
    dbname = name
  )
  conn
}

#' Command line interface to run megadapt
#'
#' @export
#' @param args command line args passed to the cli. By default takes directly from `commandArgs`
cli_root <- function(args = commandArgs(TRUE)) {
  cli <- cli_create()
  parsed <- cli$parse_args(args = args)
  conn <- cli_db_connect(parsed$db_config)
  tryCatch({
    switch(
      parsed$command,
      run = cli_run(conn = conn, experiment = parsed$experiment, id = parsed$id, study_area = parsed$study_area),
      grid = cli_grid(conn = conn, args = parsed),
      vbsa = cli_vbsa(conn = conn, args = parsed)
    )
  }, finally = DBI::dbDisconnect(conn))
}

cli_run <- function(conn, experiment, id, study_area) {
  params_run(
    conn = conn,
    experiment_name = experiment,
    id = id,
    study_area_path = study_area)
}

cli_grid <- function(conn, args) {
  if (args$grid_command != 'setup') {
    stop('Setup is the only valid grid command')
  }
  cli_grid_setup(
    conn = conn,
    experiment_config = args$experiment_config,
    study_area = args$study_area,
    db_config = args$db_config)
}

cli_grid_setup <- function(conn, experiment_config, study_area, db_config) {
  if (!fs::file_exists(experiment_config)) {
    stop(glue::glue('File {experiment_config} does not exist'))
  }

  fh <- file(experiment_config)
  config <- jsonlite::fromJSON(fh)
  experiment_table_append(
    conn = conn,
    name = config$name,
    title = config$title,
    description = config$description,
    author_name = config$author_name)

  if (config$strategy != 'cartesian')  {
    stop('Cartesian strategy is currently the only valid strategy')
  }

  name <- config$name
  overrides <- config$overrides
  params_config <- megadapt_config_create(overrides)
  params_config$rep <- seq_len(config$n_reps)
  params_config$year <- config$year
  params_config$n_steps <- config$n_steps

  flattened <- config_flatten(params_config)
  params_df <- params_cartesian_create(flattened)

  params_table_create(conn = conn, experiment_name = name, df = params_df)
  params_tbl <- dplyr::tbl(conn, glue::glue('{name}_param'))
  result_condor_submit_create(executable = '/usr/local/bin/singularity',
                              experiment_name = config$name,
                              params_tbl = params_tbl,
                              study_area_path = study_area,
                              db_config = db_config)
}

cli_vbsa <- function(conn, args) {
  if (is.null(args$vbsa_command)) {
    stop('must have a vbsa subcommand')
  }
  switch(
    parsed$vbsa_command,
    setup = cli_vbsa_setup(conn = conn, experiment_config = parsed$experiment_config),
    reduce = cli_vbsa_reduce(conn = conn, experiment = parsed$experiment)
  )
}

cli_vbsa_setup <- function(conn, experiment_config) {
  if (!fs::file_exists(experiment_config)) {
    stop(glue::glue('File {experiment_config} does not exist'))
  }

  fh <- file(experiment_config)
  config <- jsonlite::fromJSON(fh)
  experiment_table_append(
    conn = conn,
    name = config$name,
    title = config$title,
    description = config$description,
    author_name = config$author_name,
    date_created = lubridate::now())
  if (config$strategy != 'vbsa') stop('VBSA strategy is currently the only valid strategy')
  # populate params table with vbsa values here
}

cli_vbsa_reduce <- function(conn, experiment) {
  # select from experiment table to get results table
  # call appropriate vbsa function on results to get a sensitivity analysis report
}
