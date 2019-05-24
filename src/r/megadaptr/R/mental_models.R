create_mental_models <- function(potable_water_sacmex_limit_strategy,
                                 sewer_water_sacmex_limit_strategy,
                                 resident_limit_strategy,
                                 year,
                                 study_data) {
  # alternatives
  # criteria
  # by system (potable, sewer) for sacmex

  potable_water_sacmex_limit <-
    get_limit_df(potable_water_sacmex_limit_strategy,
                 year = year,
                 study_data = study_data)
  sewer_water_sacmex_limit <-
    get_limit_df(sewer_water_sacmex_limit_strategy,
                 year = year,
                 study_data = study_data)
  resident_limit <-
    get_limit_df(resident_limit_strategy,
                 year = year,
                 study_data = study_data)

  potable_alternatives_sacmex <- potable_water_sacmex_limit %>%
    dplyr::filter(cluster_name == 'Alternatives') %>%
    dplyr::filter(node %in% c('Mantenimiento', 'Nueva_infraestructura'))

  sewer_alternatives_sacmex <- sewer_water_sacmex_limit %>%
    dplyr::filter(cluster_name == 'Alternatives')

  potable_criteria_sacmex <- potable_water_sacmex_limit %>%
    dplyr::filter(cluster_name != 'Alternatives')
  sewer_criteria_sacmex <- sewer_water_sacmex_limit %>%
    dplyr::filter(cluster_name != 'Alternatives')

  # alternatives
  # criteria
  # for residents

  resident_alternatives <- resident_limit %>%
    dplyr::filter(cluster_name == 'Acciones')
  resident_criteria <- resident_limit %>%
    dplyr::filter(cluster_name != 'Acciones')

  # criteria values
  criteria_sacmcx_ab <- potable_criteria_sacmex$value
  names(criteria_sacmcx_ab) <- potable_criteria_sacmex$node
  criteria_sacmcx_d <- sewer_criteria_sacmex$value
  names(criteria_sacmcx_d) <- sewer_criteria_sacmex$node

  alternative_weights_s <- potable_alternatives_sacmex$value
  names(alternative_weights_s) <- potable_alternatives_sacmex$node
  alternative_weights_d <- sewer_alternatives_sacmex$value
  names(alternative_weights_d) <- sewer_alternatives_sacmex$node

  criteria_residents_iz <- resident_criteria$value
  names(criteria_residents_iz) <- resident_criteria$node
  alternative_weights_iz <- resident_alternatives$value
  names(alternative_weights_iz) <- resident_alternatives$node

  list(
    sacmcx = list(
      criteria = list(ab = criteria_sacmcx_ab,
                      d = criteria_sacmcx_d),
      alternative_weights = list(d = alternative_weights_d,
                                 s = alternative_weights_s)
    ),
    residents = list(
      criteria = list(iz = criteria_residents_iz),
      alternative_weights = list(iz = alternative_weights_iz)
    )
  )
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

create_limit_df <-
  function(weighted_matrix_meta, tolerance = 1e-10) {
    # calculates A^p (matrix multiplied p times with itself)
    # inputes: A - real-valued square matrix, p - natural number.
    # output:  A^p
    ##add while loop using tolerance
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
  function(mental_model, year, study_data) {
    mental_model$limit_dfs[[mental_model$limit_df_picker(year, study_area)]]
  }

file_constant_mental_model_strategy <- function(path, cluster) {
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

create_constant_mental_model_strategies <- function() {
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
    potable_water_sacmex_limit_strategy = file_constant_mental_model_strategy(
      mm_file_path('potable_water_sacmex_unweighted_stage1.csv'),
      cluster = potable_water_cluster
    ),
    sewer_water_sacmex_limit_strategy = file_constant_mental_model_strategy(
      mm_file_path('sewer_water_sacmex_unweighted_stage1.csv'),
      cluster = NULL
    ),
    resident_limit_strategy = file_constant_mental_model_strategy(mm_file_path('resident_unweighted.csv'), cluster = resident_cluster)
  )
}
