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
    list(
      sacmcx = sacmex_mental_model,
      residents = resident_mental_model
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

mental_model_file_constant_strategy_create <- function(path, cluster) {
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

mental_model_constant_strategies <- function() {
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


#' A function to calculate a new set of weights to modify the supermatrix in the block "socio-hydrological risk and Environtment, specifically in the "sewer_system_sacmex_unweighted" matrix
#'@param study_data A data frame that includes precipitation volume, runoff, and the mean precipitation and the mean runoff
#'@return A list of weights to change the block "socio-hydrological risk and Environtment" in the unweighted supermatrix
calculate_new_weights_double_coupling<-function(study_data){

  ponding_historic_sum_for_precipitation <- sum(study_data$resident_reports_ponding_count_mean,na.rm=T)
  flooding_historic_sum_for_precipitation<-sum(study_data$resident_reports_flooding_count_mean,na.rm=T)

  ponding_historic_sum_for_runnof <- sum(study_data$resident_reports_ponding_count_mean[which(study_data$runoff_bin==1)],na.rm=T)
  flooding_historic_sum_for_runnof<-sum(study_data$resident_reports_flooding_count_mean[which(study_data$runoff_bin==1)],na.rm=T)


  ponding_current_sum_for_precipitation <- sum(study_data$ponding_index,na.rm=T)
  flooding_current_sum_for_precipitation<-sum(study_data$flooding_index,na.rm=T)

  ponding_current_sum_for_runnof <- sum(study_data$ponding_index[which(study_data$runoff_bin==1)],na.rm=T)
  flooding_current_sum_for_runnof<-sum(study_data$flooding_index[which(study_data$runoff_bin==1)],na.rm=T)


  delta_ponding_precipitation=(ponding_current_sum_for_precipitation-ponding_historic_sum_for_precipitation)/ponding_historic_sum_for_precipitation
  delta_flooding_precipitation=(flooding_current_sum_for_precipitation-flooding_historic_sum_for_precipitation)/flooding_historic_sum_for_precipitation

  delta_ponding_runoff=(ponding_current_sum_for_runnof-ponding_historic_sum_for_runoff)/ponding_historic_sum_for_runoff
  delta_flooding_runoff=(flooding_current_sum_for_runoff-flooding_historic_sum_for_runoff)/flooding_historic_sum_for_runoff

  weight_ponding_precipitation=delta_ponding_precipitation/(delta_ponding_precipitation+delta_flooding_precipitation)
  weight_flooding_precipitation=1-weight_ponding_precipitation

  weight_ponding_runoff=delta_ponding_runoff/(delta_ponding_runoff+delta_flooding_runoff)
  weight_flooding_runoff=1-weight_ponding_runoff

  list("weight_ponding_precipitation"=weight_ponding_precipitation,
       "weight_flooding_precipitation"=weight_flooding_precipitation,
       "weight_ponding_runoff"=weight_ponding_runoff,
       "weight_flooding_runoff"=weight_flooding_runoff)
}

#' A function to modify the values in a block in an unweighted supermatrix.
#'
#' @param path_td a path to the data
#' @param unweighted_supermatrix  and unweighted supermatrix
#' @param block the block of the unweighted supermatrix to by modified
#' @param col_to_modify the column in the block where the values will be changed
#' @param values the new values that will replace the old weights.
#' @return A modified unweigted supermatrix
modify_block_row=function(path_td, unweighted_supermatrix, block, col_to_modify, values){
  if(sum(values)!=1)stop("values must sum to 1")
  UWM=read.csv(paste(path_td),sep=",",skip = 1,header = T)
  y_cors=which(UWM$X!="")
  x_cors=2+y_cors
  index = 1
  category_coors <- list()
  for(i in y_cors){
    index <- index + 1
    if (is.na(y_cors[index])){
      category_coors[[paste(index-1)]] <- c(i,nrow(UWM))
    }else{
      category_coors[[paste(index-1)]] <- c(i,y_cors[index]-1)
    }
  }
  a <- category_coors[[paste(block[1])]][1]
  b <- category_coors[[paste(block[1])]][2]
  cc <- category_coors[[paste(block[2])]][1] + col_to_modify - 1
  browser()
  if(length(values)!=length(a:b))stop("number of rows to change must be the same length as the length of the col_to_modify column in block")

  unwaited_supermatrix[a:b, cc] <- values
  return(unwaited_supermatrix)
  vec_cluster=rep(MM_WaterOperator_C$V2,  diff(c(y_cors,(length(UWM$X)+1))))
  weighted_supermatrix=sweep(unwaited_supermatrix,MARGIN = 2,vec_cluster,FUN = '*')

}
