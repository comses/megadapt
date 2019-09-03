cli_create <- function() {
  root <-
    argparse::ArgumentParser(description = 'Run the megadapt model')
  root$add_argument('--db-config', help = 'JSON config file used to connect to db', default='db.json')

  subparser <-
    root$add_subparsers(help = 'Subcommands', dest = 'command')

  grid_root <- subparser$add_parser('grid', help = 'Grid Search')
  grid_subparser <- grid_root$add_subparsers(help = 'Grid Search subcommands', dest = 'grid_command')

  grid_setup <- grid_subparser$add_parser('setup', help = 'Setup a Grid Search experiment')
  grid_setup$add_argument('--experiment-config', help = 'JSON file describing the model setup', required=TRUE)
  grid_setup$add_argument('--study-area', help = 'Path to study area data', default=data_dir('censusblocks', 'megadapt_wgs84_v5.gpkg'))

  grid_run <- grid_subparser$add_parser('run', help = 'Run the Megadapt model in Grid Search mode')
  grid_run$add_argument('--experiment', help = 'Name of the experiment', required=TRUE)
  grid_run$add_argument('--id', help = 'Row ID of the params combination', required=TRUE)
  grid_run$add_argument('--study-area', help = 'Path to study area data', default=data_dir('censusblocks', 'megadapt_wgs84_v5.gpkg'))

  vbsa_root <-
    subparser$add_parser('vbsa', help = 'Variance Based Sensitivity Analysis')
  vbsa_subparser <-
    vbsa_root$add_subparsers(help = 'VBSA subcommands', dest = 'vbsa_command')

  vbsa_run <- vbsa_subparser$add_parser('run', help = 'Run the Megadapt model in VBSA mode')
  vbsa_run$add_argument('--experiment-config', help = 'JSON file describing the model setup', required=TRUE)
  vbsa_run$add_argument('--id', help = 'Row ID of the params combination', required=TRUE)
  vbsa_run$add_argument('--params', help = 'List of the params combination', required=TRUE)
  vbsa_run$add_argument('--sample_n', help = 'Row number from the input matrix', required=TRUE)
  vbsa_run$add_argument('--ABMat', help = 'Slice number from the input matrix', required=TRUE)

  vbsa_setup <-
    vbsa_subparser$add_parser('setup', help = 'Setup a VBSA experiment')
  vbsa_setup$add_argument('--experiment-config', help = 'JSON file describing the model setup', required=TRUE)

  vbsa_reduce <-
    vbsa_subparser$add_parser('reduce', help = 'Reduce the Megadapt model runs and return a sensitivity analysis report')
  vbsa_reduce$add_argument('--experiment-config', help = 'Name of the experiment to collect results from', required=TRUE)

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
      grid = cli_grid(conn = conn, args = parsed),
      vbsa = cli_vbsa(conn = conn, args = parsed)
    )
  }, finally = DBI::dbDisconnect(conn))
}

cli_grid <- function(conn, args) {
  if (is.null(args$grid_command)) {
    stop('must have a grid subcommand')
  }
  switch(
    args$grid_command,
    setup = cli_grid_setup(conn = conn,
                           experiment_config = args$experiment_config,
                           study_area = args$study_area,
                           db_config = args$db_config),
    run = cli_grid_run(conn = conn,
                       experiment = args$experiment,
                       id = args$id,
                       study_area = args$study_area)
  )
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

cli_grid_run <- function(conn, experiment, id, study_area) {
  study_area <- study_area_read(study_area)
  params_run(
    conn = conn,
    experiment_name = experiment,
    id = id,
    study_area = study_area)
}


cli_vbsa <- function(conn, args) {
  if (is.null(args$vbsa_command)) {
    stop('must have a vbsa subcommand')
  }
  switch(
    args$vbsa_command,
    setup = cli_vbsa_setup(conn = conn,
                           experiment_config = args$experiment_config),
    run = cli_vbsa_run(conn = conn,
                       experiment_config = args$experiment_config,
                       id = args$id, params = args$params,
                       sample_n = args$sample_n,
                       ABMat = args$ABMat),
    reduce = cli_vbsa_reduce(conn = conn,
                             experiment = args$experiment_config)
  )
}

cli_vbsa_setup <- function(conn, experiment_config) {
  if (!fs::file_exists(experiment_config)) {
    stop(glue::glue('File {experiment_config} does not exist'))
  }

  config <- jsonlite::read_json(experiment_config, simplifyVector = TRUE)
  experiment_table_append(
    conn = conn,
    name = config$experiment_name,
    title = config$title,
    description = config$description,
    author_name = config$author_name)

  # populate params table with vbsa values here
  SA_conditions <- config$SA_conditions
  SA_params <- config$SA_params
  megadapt_conds <- config$megadapt_conds

  abs <- megadaptr:::createLinearMatrices(SA_conditions,SA_params)

  R.rsp::rfile("../submit/run.dag.rsp", args = list(ABMats = abs))

  long_abs <- megadaptr:::melt_with_info(abs)

  DBI::dbWriteTable(conn = conn,
               name = config$params_table,
               value = long_abs,
               row.names = FALSE,
               append = FALSE,
               overwrite = TRUE)

  return(abs)
}

cli_vbsa_run <- function(conn, experiment_config, id, params, sample_n, ABMat) {

  experiment_c <- jsonlite::read_json(experiment_config, simplifyVector = TRUE)
  megadapt_conds <- experiment_c$megadapt_conds
  params_list <- eval(parse(text = params))

  one_run_results <- megadaptr:::one_megadapt_superficial_params_simulator(params_list, megadapt_conds)

  # one_run_results <- cbind(one_run_results, "job_id" = id)
  # one_run_results <- cbind(one_run_results, "sample" = sample_n)
  # one_run_results <- cbind(one_run_results, "matrix" = ABMat)
  one_run_results$job_id <- id
  one_run_results$sample <- sample_n
  one_run_results$matrix <- ABMat

  DBI::dbWriteTable(conn = conn,
               name = experiment_c$results_table,
               value = one_run_results,
               row.names = FALSE,
               append = TRUE)
  print("here")
}

cli_vbsa_reduce <- function(conn, experiment_config) {

  experiment_c <- jsonlite::read_json(experiment_config, simplifyVector = TRUE)
  Ys_list <- DBI::dbReadTable(conn, experiment_c$results_table, row.names = FALSE)

  target_stats <- colnames(Ys_list)[colnames(Ys_list)!="community" & colnames(Ys_list)!="job_id" & colnames(Ys_list)!="sample" & colnames(Ys_list)!="matrix"]
  communities <- unique(Ys_list$community)

  sample_sizes <- 2^((experiment_c$SA_conditions$exp_min):(experiment_c$SA_conditions$exp_max))
  max_sample_size <- 2^(experiment_c$SA_conditions$exp_max)
  k <- length(experiment_c$SA_params)
  param_names <- NULL
  for (pnum in 1:length(experiment_c$SA_params)) {
    param_names[pnum] <- experiment_c$SA_params[[pnum]]$name
  }

  SA_results <- data.frame(sample_size=numeric(),
                           input_parameter=character(),
                           target_statistic=character(),
                           community=character(),
                           outcome_name=character(),
                           value=numeric())
  long_outs <- data.frame(sample_size=numeric(),
                          input_parameter=character(),
                          target_statistic=character(),
                          community=character(),
                          outcome_name=character(),
                          value=numeric())

  for (acommunity in communities) {
    for (target_stat in target_stats) {
      Y1 <- subset(Ys_list, community == acommunity, select = c(target_stat, "job_id", "sample", "matrix"))
      sorted_Y1 <- Y1[order(Y1$job_id),]

      Y <- reshape2::acast(sorted_Y1, sample ~ matrix, value.var = target_stat)

      for (sample_s in sample_sizes) {
        Si <- calc_Si(Y, sample_s, k)
        STi <- calc_STi(Y, sample_s, k)

        new_Si_results <- data.frame(sample_size = sample_s,
                                     input_parameter = param_names,
                                     target_statistic = target_stat,
                                     community = acommunity,
                                     outcome_name = "first_order_sensitivity_index",
                                     value = Si)
        new_STi_results <- data.frame(sample_size = sample_s,
                                      input_parameter = param_names,
                                      target_statistic = target_stat,
                                      community = acommunity,
                                      outcome_name = "total_order_sensitivity_index",
                                      value = STi)
        new_SA_results <- rbind(new_Si_results, new_STi_results)

        SA_results <- rbind(SA_results, new_SA_results)
      }

      summary_outs <- megadaptr:::appl_summary_statistics(matr = Y, summ_stats = experiment_c$SA_conditions$summary_stats)

      new_outs <- data.frame(sample_size = max_sample_size,
                             input_parameter = "Overall",
                             target_statistic = target_stat,
                             community = acommunity,
                             outcome_name = names(summary_outs),
                             value = summary_outs)

      long_outs <- rbind(long_outs, new_outs)
    }
  }

  final_output <- rbind(SA_results, long_outs)

  table_name <- paste("SA_results_", experiment_c$results_table)

  DBI::dbWriteTable(conn = conn,
                    name = table_name,
                    value = final_output,
                    overwrite = TRUE)

  saveRDS(final_output, "SAresults")

}
