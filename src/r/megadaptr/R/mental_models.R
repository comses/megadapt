mental_model_sacmex_create <-
  function(potable_water_sacmex_limit_strategy,
           sewer_water_sacmex_limit_strategy,
           year,
           study_data) {
    potable_water_sacmex_limit <-
      get_limit_df(potable_water_sacmex_limit_strategy,
                   year = year,
                   study_data = study_data)

    potable_alternatives_sacmex <- potable_water_sacmex_limit %>%
      dplyr::filter(cluster_name == 'Alternatives') %>%
      dplyr::filter(node %in% c('Mantenimiento', 'Nueva_infraestructura'))
    alternative_weights_s <- potable_alternatives_sacmex$value
    names(alternative_weights_s) <- potable_alternatives_sacmex$node

    potable_criteria_sacmex <- potable_water_sacmex_limit %>%
      dplyr::filter(cluster_name != 'Alternatives')
    criteria_sacmcx_ab <- potable_criteria_sacmex$value
    names(criteria_sacmcx_ab) <- potable_criteria_sacmex$node

    sewer_water_sacmex_limit <-
      get_limit_df(sewer_water_sacmex_limit_strategy,
                   year = year,
                   study_data = study_data)

    sewer_alternatives_sacmex <- sewer_water_sacmex_limit %>%
      dplyr::filter(cluster_name == 'Alternatives')
    alternative_weights_d <- sewer_alternatives_sacmex$value
    names(alternative_weights_d) <- sewer_alternatives_sacmex$node

    sewer_criteria_sacmex <- sewer_water_sacmex_limit %>%
      dplyr::filter(cluster_name != 'Alternatives')
    criteria_sacmcx_d <- sewer_criteria_sacmex$value
    names(criteria_sacmcx_d) <- sewer_criteria_sacmex$node

    list(
      criteria = list(ab = criteria_sacmcx_ab,
                      d = criteria_sacmcx_d),
      alternative_weights = list(d = alternative_weights_d,
                                 s = alternative_weights_s)
    )
  }

mental_model_resident_create <- function(resident_limit_strategy,
                                         year = year,
                                         study_data = study_data) {
  resident_limit <-
    get_limit_df(resident_limit_strategy,
                 year = year,
                 study_data = study_data)

  resident_alternatives <- resident_limit %>%
    dplyr::filter(cluster_name == 'Acciones')
  alternative_weights_iz <- resident_alternatives$value
  names(alternative_weights_iz) <- resident_alternatives$node

  resident_criteria <- resident_limit %>%
    dplyr::filter(cluster_name != 'Acciones')
  criteria_residents_iz <- resident_criteria$value
  names(criteria_residents_iz) <- resident_criteria$node

  list(
    criteria = list(iz = criteria_residents_iz),
    alternative_weights = list(iz = alternative_weights_iz)
  )
}

create_mental_models <-
  function(potable_water_sacmex_limit_strategy,
           sewer_water_sacmex_limit_strategy,
           resident_limit_strategy,
           year,
           study_data) {
    sacmex_mental_model <- mental_model_sacmex_create(
      potable_water_sacmex_limit_strategy = potable_water_sacmex_limit_strategy,
      sewer_water_sacmex_limit_strategy = sewer_water_sacmex_limit_strategy,
      year = year,
      study_data = study_data
    )

    resident_mental_model <- mental_model_resident_create(
      resident_limit_strategy = resident_limit_strategy,
      year = year,
      study_data = study_data
    )
    list(sacmcx = sacmex_mental_model,
         residents = resident_mental_model)
  }

read_cluster_matrix <- function(path) {
  #' Read a cluster matrix located at a path
  #'
  #' @export
  #' @param path where the cluster matrix csv file is located
  as.matrix(suppressWarnings(readr::read_csv(path)[, -1]))
}

read_unweighted_matrix <- function(path) {
  df <-
    suppressWarnings(readr::read_csv(path, skip = 2, col_names = FALSE))
  if (nrow(df) + 2 != ncol(df)) {
    stop("Unweighted matrix csv must be square")
  }
  labels <- df[, c(1, 2)]
  data <- as.matrix(df[, -c(1, 2)])
  not_na_indices <- which(!is.na(labels[, 1]))
  cluster_name_change_indices <-
    c(not_na_indices, nrow(labels) + 1) - 1
  cluster_sizes <-
    (cluster_name_change_indices - dplyr::lag(cluster_name_change_indices))[-1]
  labels[, 1] <- rep(labels[not_na_indices, ][[1]], cluster_sizes)
  colnames(labels) <- c('cluster_name', 'node')
  list(labels = labels,
       cluster_sizes = cluster_sizes,
       data = data)
}

create_weighted_matrix <-
  function(unweighted_matrix_meta,
           cluster) {
    if (is.null(cluster)) {
      warning('no cluster matrix given so assuming equal weights')
      n <- length(unweighted_matrix_meta$cluster_size)
      cluster <- matrix(1 / n, nrow = n, ncol = n)
    }
    cluster_sizes <- unweighted_matrix_meta$cluster_sizes
    unweighted_matrix <- unweighted_matrix_meta$data
    inds <- rep(seq(cluster_sizes), cluster_sizes)
    weighted_element_size <- cluster[inds, inds] * unweighted_matrix
    list(data = t(
      t(weighted_element_size) / apply(weighted_element_size, 2, sum)
    ),
    labels = unweighted_matrix_meta$labels)

  }

#' A function to obtain the limit vector from a weighted supermatrix.
#'
#' @param weighted_matrix_meta A - real-valued square matrix, p - natural number.
#' @param tolerance the tolerance of the difference between columns after the matrix is elevated to p
#' @note calculate A^p, where A is a weighted supermatrix
create_limit_df <-
  function(weighted_matrix_meta, tolerance = 1e-10) {
    weighted_matrix <- weighted_matrix_meta$data
    W_matrix_B = weighted_matrix
    while (any(abs(W_matrix_B[, 1] - W_matrix_B[, 2]) > tolerance)) {
      W_matrix_B = W_matrix_B %*% weighted_matrix
    }
    dplyr::bind_cols(labels = weighted_matrix_meta$labels,
                     value = W_matrix_B[, 1])
  }

create_limit_df_from_unweighted_matrix_file <-
  function(path, cluster, tolerance = 1e-8) {
    unweighted_matrix_meta <- read_unweighted_matrix(path)
    weighted_matrix_meta <-
      create_weighted_matrix(unweighted_matrix_meta = unweighted_matrix_meta,
                             cluster = cluster)

    create_limit_df(weighted_matrix_meta = weighted_matrix_meta, tolerance = tolerance)
  }

find_cluster_given_weighted_and_unweighted <-
  function(weighted_matrix_path,
           unweighted_matrix_path) {
    um <- read_unweighted_matrix(unweighted_matrix_path)
    wm <- read_unweighted_matrix(weighted_matrix_path)
    normalized_um <- t(t(um$data) / colSums(um$data))
    wm$data  / normalized_um
  }

file_mental_model_strategy <-
  function(paths, limit_df_picker, cluster) {
    limit_dfs <-
      lapply(paths,
             create_limit_df_from_unweighted_matrix_file,
             cluster = cluster)

    structure(
      list(limit_dfs = limit_dfs,
           limit_df_picker = limit_df_picker),
      class = c("file_mental_model", "list")
    )
  }



get_limit_df <- function(mental_model, year, study_data) {
  UseMethod('get_limit_df', mental_model)
}

get_limit_df.file_mental_model <-
  mm_file_path <-
  function(path)
    system.file(
      fs::path('rawdata', 'mental_models', path),
      package = 'megadaptr',
      mustWork = TRUE
    )
  function(mental_model, year, study_data) {
    mm_file_path(mental_model$limit_dfs[[mental_model$limit_df_picker(year, study_area)]])
  }

mental_model_file_constant_strategy_create <-
  function(path, cluster) {
    #' Use a single constant mental model for the simulation
    #'
    #' @export
    #' @param path file path to the unweighted mental model matrix
    #' @param cluster a cluster mental model matrix. If the cluster is null then
    #' equal weights are assumed.
    structure(
      list(limit_df = create_limit_df_from_unweighted_matrix_file(path, cluster)),
      class = c("file_constant_mental_model", "list")
    )
  }

get_limit_df.file_constant_mental_model <-
  function(mental_model, year, study_data) {
    mental_model$limit_df
  }

#' Create constant mental model strategies
#'
#' @export
#' @return potable, sewer and household mental models
mental_model_constant_strategies <- function(config) {
  mm_file_path <-
    function(path)
      system.file(
        fs::path('rawdata', 'mental_models', path),
        package = 'megadaptr',
        mustWork = TRUE
      )

  potable_water_cluster <-
    read_cluster_matrix(mm_file_path('potable_water_cluster_sacmex.csv'))
  resident_cluster <-
    read_cluster_matrix(mm_file_path('resident_cluster.csv'))

  mental_model_strategies = list(
    potable_water_sacmex_limit_strategy = mental_model_file_constant_strategy_create(
      mm_file_path('potable_water_sacmex_unweighted_stage1.csv'),
      cluster = potable_water_cluster
    ),
    sewer_water_sacmex_limit_strategy = mental_model_file_constant_strategy_create(
      mm_file_path('sewer_water_sacmex_unweighted_stage1.csv'),
      cluster = NULL
    ),
    resident_limit_strategy = mental_model_file_constant_strategy_create(mm_file_path('resident_unweighted.csv'), cluster = resident_cluster)
  )
}

#' Create constant mental model strategies
#'
#' @export
#' @return potable, sewer and household mental models
mental_model_time_series_strategies <- function(config) {
  mm_file_path <-
    function(path)
      system.file(
        fs::path('rawdata', 'mental_models', path),
        package = 'megadaptr',
        mustWork = TRUE
      )
  potable_water_cluster <-
    read_cluster_matrix(mm_file_path('potable_water_cluster_sacmex.csv'))
  resident_cluster <-
    read_cluster_matrix(mm_file_path('resident_cluster.csv'))

  mental_model_strategies = list(
    potable_water_sacmex_limit_strategy = file_mental_model_strategy(
      paths = config$potable,
      limit_df_picker = function(year, study_area) {
        if (year > config$change_year) {
          2
        } else{
          1
        }
      },

      cluster = potable_water_cluster
    ),
    sewer_water_sacmex_limit_strategy = file_mental_model_strategy(
      paths = config$sewer,
      limit_df_picker = function(year, study_area) {
        if (year > config$change_year) {
          2
        } else{
          1
        }
      },

      cluster = sewer_water_cluster
    ),
    resident_limit_strategy = mental_model_file_constant_strategy_create(mm_file_path('resident_unweighted.csv'), cluster = resident_cluster)
  )
}

#' A function to calculate a new set of weights to modify the supermatrix in the
#' block (risk, Environtment) in the "sewer_system_sacmex_unweighted" matrix.
#' @param unweighted_matrix_meta An object with the elements of the unweighted matrix (labels, values, and dimension).
#' @param study_data A data frame that includes precipitation volume, runoff, and the mean precipitation and the mean runoff.
#' @return A matrix of weights of the same dimension as the block to change in the supermatrix.
mental_model_update_risks <-
  function(unweighted_matrix_meta, study_data) {
    get_um_value <- function(row_label, col_label) {
      labels <- unweighted_matrix_meta$labels$node
      row_inds <- which(labels == row_label)
      col_inds <- which(labels == col_label)
      unweighted_matrix_meta$data[[row_inds, col_inds]]
    }

    ponding_historic_sum_for_precipitation <-
      sum(study_data$resident_reports_ponding_count_mean, na.rm = T)
    flooding_historic_sum_for_precipitation <-
      sum(study_data$resident_reports_flooding_count_mean, na.rm = T)

    ponding_historic_sum_for_runoff <-
      sum(study_data$resident_reports_ponding_count_mean[which(study_data$runoff_presence ==
                                                                 1)], na.rm = T)
    flooding_historic_sum_for_runoff <-
      sum(study_data$resident_reports_flooding_count_mean[which(study_data$runoff_presence ==
                                                                  1)], na.rm = T)

    ponding_current_sum_for_precipitation <-
      sum(study_data$ponding_event_count, na.rm = T)
    flooding_current_sum_for_precipitation <-
      sum(study_data$flooding_event_count, na.rm = T)

    ponding_current_sum_for_runoff <-
      sum(study_data$ponding_event_count[which(study_data$runoff_presence == 1)], na.rm =
            T)
    flooding_current_sum_for_runoff <-
      sum(study_data$flooding_event_count[which(study_data$runoff_presence == 1)], na.rm =
            T)

    weight_precipitation <-
      c(
        ponding_current_sum_for_precipitation / ponding_historic_sum_for_precipitation * get_um_value('Encharcamientos', 'Precipitacion'),
        flooding_current_sum_for_precipitation / flooding_historic_sum_for_precipitation * get_um_value('Inundaciones', 'Precipitacion')
      )
    weight_precipitation <-
      weight_precipitation /  sum(weight_precipitation)

    weight_runoff <-
      c(
        ponding_current_sum_for_runoff / ponding_historic_sum_for_runoff * get_um_value('Encharcamientos', 'Escurrimiento'),
        flooding_current_sum_for_runoff / flooding_historic_sum_for_runoff * get_um_value('Inundaciones', 'Escurrimiento')
      )
    weight_runoff <- weight_runoff / sum(weight_runoff)

    block <-
      matrix(
        c(0.0, 0.0,
          weight_runoff,
          0.9, 0.1,
          weight_precipitation),
        nrow = 2,
        ncol = 4
      )
    colnames(block) <-
      c('Basura', 'Escurrimiento', 'Hundimientos', 'Precipitacion')
    rownames(block) <- c('Encharcamientos', 'Inundaciones')
    print(block)
    block
  }

mental_model_coupled_create <- function(path, cluster) {
  #' This function creates a mental mode object from superDecision output for running megadapt model with double coupling
  #'
  #' @export
  #' @param path file path to the unweighted mental model matrix
  #' @param cluster a cluster mental model matrix. If the cluster is null then
  #' equal weights are assumed.
  #' @return A mental model object with a list taht includes a limit vector, an unweighted matrix and, a cluster matrix.
  structure(
    list(
      limit_df = create_limit_df_from_unweighted_matrix_file(path, cluster),
      unweighted_matrix_meta = read_unweighted_matrix(path),
      cluster = cluster
    ),
    class = c("mental_model_coupled", "list")
  )
}

#' Obtain a limit vector from a weighted matrix
#' @param mental_model A mental model object of class "mental model"
#' @param year remove?
#' @param study_data A data frame of the study area.
#' @return A limit vector object.
get_limit_df.mental_model_coupled <-
  function(mental_model, year, study_data) {
    w <-
      mental_model_update_risks(mental_model$unweighted_matrix_meta, study_data)
    labels <-
      mental_model$unweighted_matrix_meta$labels$cluster_name
    col_cluster_inds <- which(labels == "Biof\xedsico")
    row_cluster_inds <- which(labels == "Riesgos_poblacion")

    mental_model$unweighted_matrix_meta$data[row_cluster_inds, col_cluster_inds] <-
      w
    weighted_matrix_meta <-
      create_weighted_matrix(
        unweighted_matrix_meta = mental_model$unweighted_matrix_meta,
        cluster = mental_model$cluster
      )
    create_limit_df(weighted_matrix_meta)
  }

#' Mental model default constructuctor few sewer model with feedback
#'
#' @export
mental_model_sacmex_coupled_strategies <- function(config) {
  mm_file_path <-
    function(path)
      system.file(
        fs::path('rawdata', 'mental_models', path),
        package = 'megadaptr',
        mustWork = TRUE
      )

  potable_water_cluster <-
    read_cluster_matrix(mm_file_path('potable_water_cluster_sacmex.csv'))
  resident_cluster <-
    read_cluster_matrix(mm_file_path('resident_cluster.csv'))

  mental_model_strategies = list(
    potable_water_sacmex_limit_strategy = mental_model_file_constant_strategy_create(
      mm_file_path('potable_water_sacmex_unweighted_stage1.csv'),
      cluster = potable_water_cluster
    ),
    sewer_water_sacmex_limit_strategy = mental_model_coupled_create(
      mm_file_path('sewer_water_sacmex_unweighted_stage1.csv'),
      cluster = NULL
    ),
    resident_limit_strategy = mental_model_file_constant_strategy_create(mm_file_path('resident_unweighted.csv'), cluster = resident_cluster)
  )
}

mental_model_deserialize <- function(config) {
  config <- do.call(mental_model_config_create, config)
  env <- new.env(parent = emptyenv())
  env$time_series <- mental_model_time_series_strategies
  env$coupled <- mental_model_sacmex_coupled_strategies
  env$constant <- mental_model_constant_strategies
  strategy <- get(config$strategy, envir = env)
  strategy(config$config)
}

mental_model_config_create <-
  function(strategy = 'coupled',
           config = list()) {
    strategy <-
      intersect(strategy, c('coupled', 'constant', 'time_series'))
    list(strategy = strategy,
         config = config)
  }
