cli_create <- function() {
  root <-
    argparse::ArgumentParser(description = 'Run the megadapt model')

  subparser <-
    root$add_subparsers(help = 'Subcommands', dest = 'command')

  run <- subparser$add_parser('run', help = 'Run the Megadapt model')
  run$add_argument('--experiment', help = 'Name of the experiment', required=TRUE)
  run$add_argument('--id', help = 'Row ID of the params combination', required=TRUE)

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

cli_db_connect <- function(db_config) {
  defaults <- c(DB_NAME='megadapt', DB_HOST='tawa', DB_PORT=5432, DB_USER='fidel')
  for (db_key in names(db_config)) {
    if (db_config[db_key] == '') {
      db_config[db_key] = defaults[db_key]
    }
  }
  drv <- RPostgreSQL::PostgreSQL()
  conn <- DBI::dbConnect(
    drv,
    dbname = db_config['DB_NAME'],
    host = db_config['DB_HOST'],
    port = db_config['DB_PORT'],
    user = db_config['DB_USER']
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
  switch(
    parsed$command,
    run = cli_run(experiment = parsed$experiment, id = parsed$id),
    vbsa = cli_vbsa(parsed)
  )
}

cli_run <- function(experiment, id) {
  conn <- cli_db_connect(Sys.getenv(c('DB_NAME', 'DB_HOST', 'DB_USER', 'DB_PORT')))
  # run the model
  # save the sensitivity analysis results in the DB
}

cli_vbsa <- function(parsed) {
  if (is.null(parsed$vbsa_command)) {
    stop('must have a vbsa subcommand')
  }
  switch(
    parsed$vbsa_command,
    setup = cli_vbsa_setup(experiment_config = parsed$experiment_config),
    reduce = cli_vbsa_reduce(experiment = parsed$experiment)
  )
}

cli_vbsa_setup <- function(experiment_config) {
  if (!fs::file_exists(experiment_config)) {
    stop(glue::glue('File {experiment_config} does not exist'))
  }
  fh <- file(experiment_config)
  config <- jsonlite::fromJSON(fh)
  config
  # call appropriate vbsa function here
}

cli_vbsa_reduce <- function(experiment) {
  conn <- cli_db_connect(Sys.getenv(c('DB_NAME', 'DB_HOST', 'DB_USER', 'DB_PORT')))
  # select from experiment table to get results table
  # call appropriate vbsa function on results to get a sensitivity analysis report
}
