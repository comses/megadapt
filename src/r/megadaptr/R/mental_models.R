create_mental_models <- function(mm_water_operator_d_lim,
                                 mm_water_operator_s_lim,
                                 mm_iz) {
  # name criteria
  names_criteria_sacmex_s <-
    colnames(mm_water_operator_s_lim)[-c(1:5)]
  names_criteria_sacmex_d <-
    colnames(mm_water_operator_d_lim)[-c(1, 2)]

  # criteria values
  criteria_sacmcx_ab <- mm_water_operator_s_lim$Antiguedad[-c(1:5)]
  criteria_sacmcx_d <- mm_water_operator_d_lim$Antiguedad[-c(1:2)]

  # alternative names
  names_alternative_sacmex_s <-
    colnames(mm_water_operator_s_lim)[c(1:5)]
  names_alternative_sacmex_d <-
    colnames(mm_water_operator_d_lim)[c(1, 2)]

  alternative_weights_s <-
    mm_water_operator_s_lim$Antiguedad[c(1:5)]
  alternative_weights_d <-
    mm_water_operator_d_lim$Antiguedad[c(1:2)]

  names_criteria_resident_iz <- colnames(mm_iz)[-c(1:5)]
  names_alternative_Resident_iz <- colnames(mm_iz)[c(1:5)]

  criteria_residents_iz <- mm_iz$Compra.de.agua[-c(1:5)]
  alternative_weights_iz <- mm_iz$Compra.de.agua[c(1:5)]

  list(
    sacmcx = list(
      names_criteria = list(ab = names_criteria_sacmex_s,
                            d = names_criteria_sacmex_d),
      criteria = list(ab = criteria_sacmcx_ab,
                      d = criteria_sacmcx_d),
      names_alternatives = list(ab = names_alternative_sacmex_s,
                                d = names_alternative_sacmex_d),
      alternative_weights = list(d = alternative_weights_d,
                                 s = alternative_weights_s)
    ),
    residents = list(
      names_criteria = list(names_criteria_resident_iz),
      criteria = list(iz = criteria_residents_iz),
      names_alternatives = list(names_alternative_Resident_iz),
      alternative_weights = list(iz = alternative_weights_iz)
    )
  )
}

read_cluster_matrix <- function(path) {
  as.matrix(suppressWarnings(readr::read_csv(path)[,-1]))
}

read_unweighted_matrix <- function(path) {
  df <- suppressWarnings(readr::read_csv(path, skip = 2, col_names = FALSE))
  if (nrow(df) + 2 != ncol(df)) {
    stop("Unweighted matrix csv must be square")
  }
  names <- df[, c(1, 2)]
  data <- as.matrix(df[,-c(1, 2)])
  cluster_name_change_indices <-
    c(which(!is.na(names[, 1])), nrow(names) + 1) - 1
  cluster_sizes = (cluster_name_change_indices - lag(cluster_name_change_indices))[-1]
  list(names = names,
       cluster_sizes = cluster_sizes,
       data = data)
}

create_weighted_matrix <-
  function(unweighted_matrix,
           cluster_sizes,
           cluster) {
    inds <- rep(seq(cluster_sizes), cluster_sizes)
    weighted_element_size <- cluster[inds, inds] * unweighted_matrix
    t(t(weighted_element_size) / apply(weighted_element_size, 2, sum))
  }

create_limit_matrix <- function(weighted_matrix, tolerance = 1e-5) {
  # calculates A^p (matrix multiplied p times with itself)
  # inputes: A - real-valued square matrix, p - natural number.
  # output:  A^p
  ##add while loop using tolerance
  W_matrix_B = weighted_matrix
  while (any(abs(W_matrix_B[, 1] - W_matrix_B[, 2]) > tolerance)) {
    W_matrix_B = W_matrix_B %*% weighted_matrix
  }
  W_matrix_B
}

create_limit_matrix_from_unweighted_matrix_file <-
  function(path, cluster, tolerance = 1e-8) {
    unweighted_matrix_meta <- read_unweighted_matrix(path)
    weighted_matrix <-
      create_weighted_matrix(
        unweighted_matrix = unweighted_matrix_meta$data,
        cluster_sizes = unweighted_matrix_meta$cluster_sizes,
        cluster = cluster
      )
    create_limit_matrix(weighted_matrix = weighted_matrix, tolerance = tolerance)
  }

file_mental_model_strategy <- function(paths, limit_matrix_picker, cluster) {
  limit_matrices <-
    lapply(paths, create_limit_matrix_from_unweighted_matrix_file, cluster = cluster)

  structure(
    list(limit_matrices = limit_matrices,
         limit_matrix_picker = limit_matrix_picker),
    class = c("file_mental_model", "list")
  )
}

get_limit_matrix <- function(mental_model, year, study_area) {
  UseMethod('get_limit_matrix', mental_model)
}

get_limit_matrix.file_mental_model <-
  function(mental_model, year, study_area) {
    mental_model$limit_matrices[[mental_model$limit_matrix_picker(year, study_area)]]
  }
