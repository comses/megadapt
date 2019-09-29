create_config <- function(clusterID, config_name, exp_max) {
  SA_params <- list(
    p1 = list(
      name = "new_infrastructure_effectiveness_rate",
      min = 0.01,
      max = 0.3
    ),
    p2 = list(
      name = "maintenance_effectiveness_rate",
      min = 0.1,
      max = 0.3
    ),
    p3 = list(
      name = "infrastructure_decay_rate",
      min = 0.001,
      max = 1
    ),
    p4 = list(
      name = "budget",
      min = 24,
      max = 2428
    )
  )

  megadapt_conds <- list(
    exp_min = 1,
    exp_max = exp_max,
    sim_years = 40,
    municip = T,
    out_stats = c("mean","max","min"),
    out_metric_names = c("household_potable_water_vulnerability", "household_sewer_vulnerability"))

  ####### No need to modify the rest of the list

  experiment_name <- paste("sa_exponent_",megadapt_conds$exp_max,"_id_",clusterID, sep = "")

  description <- paste("Variance Based Sensitivity Analysis on", length(SA_params),"parameters and sample size of", (2^(megadapt_conds$exp_max)))

  params_table <- paste("parameters_", experiment_name, sep = "")

  results_table <- paste("results_", experiment_name, sep = "")

  SA_config <- list(SA_params = SA_params,
                    megadapt_conds = megadapt_conds,
                    experiment_name = experiment_name,
                    title = experiment_name,
                    description = description,
                    author_name = "Manuela Vanegas Ferro",
                    params_table = params_table,
                    results_table = results_table)

  jsonlite::write_json(SA_config, config_name)

}

#' Create possible input parameter combinations for running the sensitivity analysis
#'
#' @param SA_conds sensitivity analysis configuration object
#' @param SA_params sensitivity analysis input parameter sample space
#' @return input parameter combinations as an N-dimensional array
createLinearMatrices <- function(SA_conds, SA_params) {
  N <- 2 ^ SA_conds$exp_max
  k <- length(SA_params)
  param_names <- NULL
  for (pname in 1:length(SA_params)) {
    param_names[pname] <- SA_params[[pname]]$name
  }
  matrix_names <- c("A","B", param_names)

  interc <- rep(0, k)
  for (i in 1:k) {
    interc[i] <- SA_params[[i]]$min
  }

  slope <- rep(0, k)
  for (j in 1:k) {
    slope[j] <- (SA_params[[j]]$max - SA_params[[j]]$min)
  }


  S = qrng::sobol(N + 4, 2 * k, skip = 1)

  for (i in 1:(2 * k)) {
    if (i <= k) {
      S[, i] <- S[, i] * slope[i] + interc[i]
    } else {
      S[, i] = S[, i] * slope[i - k] + interc[i - k]
    }
  }


  ABMats <- array(dim = c(N, k, (k + 2)),
                  dimnames = list("sample_number" = 1:N,
                               "x_i" = param_names,
                               "matrix_name" = matrix_names))
  ABMats[, , ] <- S[1:N, 1:k]
  ABMats[, , 2] <- S[1:(N), (k + 1):(2 * k)]
  for (j in 1:k) {
    ABMats[, j, (j + 2)] <- S[1:(N), j + k]
  }
  ABMats[,,2] <- S[5:(N+4),(k+1):(2*k)] #Pairing A with B taken from 4 rows below (p.38 from book chapter)
  for (j in 1:k) {
    ABMats[,j,(j+2)] <- S[5:(N+4),j+k]
  }

  return(ABMats)

}

################################################################################################
################################################################################################

#'Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.
#'
#' @param model_f function that configures and runs the model to be analyzed
#' @param SA_conditions sensivity analysis configuration object
#' @param SA_params sensitivity analysis input parameter sample space
VBSA <- function(model_f, SA_conditions, SA_params, batchtools_resources) {

  ABMats <- createLinearMatrices(SA_conditions, SA_params)

  Y <- runSimulations(model_f = model_f,
                      SA_conditions = SA_conditions,
                      SA_params = SA_params,
                      ABMats = ABMats,
                      b_resources = batchtools_resources)

  resultss <- calcSensitivityIndices(SA_conditions = SA_conditions,
                                     SA_params = SA_params,
                                     Y = Y)

  outt <- longFormThis(outs = Y,
                       SA = resultss,
                       SA_conditions = SA_conditions,
                       megadapt_conds = megadapt_conds)

  return(outt)
}


################################################################################################
################################################################################################

runSimulations <- function (model_f, SA_params, SA_conditions, ABMats, b_resources) {

  if (SA_conditions$on_cluster) {
    host_list <- paste(unlist(read.delim("~/SA/megadapt3/src/r/scenarios/slurmhosts",header=F)))
    av_cores <- future::availableCores()
    # print("Av Cores:")
    # print(av_cores)
    # print("Hosts:")
    # print(host_list)
    future::plan(future::cluster, workers = host_list)
    # future::plan(future.batchtools::batchtools_slurm, resources = b_resources, template =  "batchtools.slurm.tmpl")
  } else {
    av_cores <- future::availableCores()
    future::plan(future::multiprocess, workers = av_cores)
  }

  if (SA_conditions$run_model) {
    # Yi <- future.apply::future_apply(ABMats, c(1, 3), function(x) model_f(x))
    Yi <- apply(ABMats, c(1, 3), function(x) model_f(x))



    if (is.null(Yi[1,1][[1]]$dims)) {

      Y_dims <- c(length(Yi[,1]),
                  length(Yi[1,]))
      Y_dimnames <- dimnames(Yi)

      Yi_metrics <- list()
      Yi_metrics_place <- 1
      for (Y_row in 1:length(Yi[,1])) {
        for (Y_col in 1:length(Yi[1,])) {
          Yi_metrics[[Yi_metrics_place]] <- Yi[Y_row, Y_col][[1]]$metrics
          Yi_metrics_place <- Yi_metrics_place + 1
        }
      }

      Y <- array(unlist(Yi_metrics),
                 dim = Y_dims,
                 dimnames = Y_dimnames)

    } else {
      Y_dims <- c(Yi[1,1][[1]]$dims,
                  length(Yi[,1]),
                  length(Yi[1,]))
      Y_extra_dims <- dimnames(Yi[1,1][[1]]$metrics)
      Y_dimnames <- c(Y_extra_dims,dimnames(Yi))

      Yi_metrics <- list()
      Yi_metrics_place <- 1
      for (Y_row in 1:length(Yi[,1])) {
        for (Y_col in 1:length(Yi[1,])) {
          Yi_metrics[[Yi_metrics_place]] <- Yi[Y_row, Y_col][[1]]$metrics
          Yi_metrics_place <- Yi_metrics_place + 1
        }
      }

      Y <- array(unlist(Yi_metrics),
                 dim = Y_dims,
                 dimnames = Y_dimnames)

      Y <- aperm(Y,c(3,4,1,2))
    }

    saveRDS(Y, "modelOuts")

  } else {
    Y <- readRDS("modelOuts")
  }

  return(Y)

}

################################################################################################
################################################################################################

megadapt_superficial_params_simulator <- function(megadapt_conds, SA_params) {

  megadapt <- megadapt_create(params = params_create(n_steps = megadapt_conds$sim_years))

  param_names <- NULL
  for (pname in 1:length(SA_params)) {
    param_names[pname] <- SA_params[[pname]]$name
  }

  summary_funcs <- as.list(megadapt_conds$out_stats)
  names(summary_funcs) <- megadapt_conds$out_stats
  out_metric_names <- megadapt_conds$out_metric_names

  function(x) {

    #Assign values to variables
    new_params <- assign_values_param_names(x, param_names, megadapt_conds)

    new_p_list <- do.call(params_create,new_params)

    # Add parameters to the megadapt object
    megadapt <- modify_megadapt_model(megadapt, new_p_list)

    #Run model and get results from last year
    results <- simulate(megadapt)

    lastT <- max(results$year)

    if (megadapt_conds$municip) {
      Vlast <- subset(results, year == lastT, select = c("geographic_id", out_metric_names))
      Vlast$Mun <- substr(Vlast$geographic_id, start = 1, stop = 5)

      metrics <- dplyr::group_by(Vlast, Mun) %>% dplyr::summarise_at(out_metric_names, summary_funcs, na.rm=TRUE)
      total <- dplyr::summarise_at(Vlast, out_metric_names, summary_funcs, na.rm=TRUE)
      total$Mun <- "Global"
      metrics <- rbind(metrics,total)
      # muns <- metrics$Mun
      muns <- community_names()
      metrics <- dplyr::select(metrics, -Mun)
      metrics_a <- simplify2array(metrics)
      dimnames(metrics_a) <- list("community" = muns,
                                 "target_statistic" = colnames(metrics))

    } else {
      Vlast <- subset(results, year == lastT, select = out_metric_names)
      metrics <- apply(Vlast, 2, function(x) mean(x, na.rm = T))
      metrics_a <- metrics
    }

    rreturn <- list(metrics = metrics_a,
                    dims = dim(metrics_a))
    return(rreturn)
  }


}

one_megadapt_superficial_params_simulator <- function(params, megadapt_conds) {

  params$n_steps <- megadapt_conds$sim_years

  params_list <- do.call(params_create, params)

  megadapt <- megadapt_create(params = params_list)

  results <- simulate(megadapt)

  return(results)
}

book_example_simulator <- function() {
  function(x) {
    y = 3 * (x[1] ^ 2) + 2 * (x[1] * x[2]) - 2 * x[3]

    return(list(metrics = y,
                dims = dim(y)))
  }
}

################################################################################################
################################################################################################

assign_values_param_names <- function(values, pnames, megadapt_conditions) {

  custom_params <- list()
  param_ind <- 1
  for (param_name in pnames) {
    if (!is.na(values[param_ind])) {
      custom_params[[param_name]] <- values[param_ind]
    }
    param_ind <- param_ind + 1
  }
  custom_params$n_steps <- megadapt_conditions$sim_years

  return(custom_params)
}

modify_megadapt_model <- function (megadapt,
                                    params,
                                    sacmex_fnss_creator = sacmex_seperate_action_budgets_fnss_create) {
  megadapt_create(params = params,
                  flooding_fnss = megadapt$flooding_fnss,
                  ponding_fnss = megadapt$ponding_fnss,
                  mental_models = megadapt$mental_models,
                  sacmex_fnss_creator = megadapt$sacmex_fnss_creator,
                  study_area = megadapt$study_area)

  return(megadapt)
}

community_names <- function() {
  muns <- c("Azcapotzalco",
            "Coyoacan",
            "Cuajimalpa_de_Morelos",
            "Gustavo_A_Madero",
            "Iztacalco",
            "Iztapalapa",
            "La_Magdalena_Contreras",
            "Milpa_Alta",
            "Alvaro_Obregon",
            "Tlahuac",
            "Tlalpan",
            "Xochimilco",
            "Benito_Juarez",
            "Cuauhtemoc",
            "Miguel_Hidalgo",
            "Venustiano_Carranza",
            "Global")

  muns
}

################################################################################################
################################################################################################

calcSensitivityIndices <- function(SA_conditions, SA_params, Y) {

  exp_min <- SA_conditions$exp_min
  exp_max <- SA_conditions$exp_max
  maxN <- 2 ^ exp_max
  k <- length(SA_params)

  param <- NULL
  for (i in 1:length(SA_params)) {
    param[i] <- SA_params[[i]]$name
  }
  sample_size <- as.character(2^(exp_min:exp_max))

  Y_dims<-dim(Y)

  results_si <- array()
  results_sti <-array()

  margins_to_apply <- 1:length(Y_dims)
  margins_to_apply <- margins_to_apply[-1:-2]

  if (length(margins_to_apply) == 0) {
    for (i in exp_min:exp_max) {
      N <- 2 ^ i

      Sis <- calc_Si(Y, N, k)
      STis <- calc_STi(Y, N, k)
      results_si[1:k, (i - exp_min + 1)] <-Sis
      results_sti[1:k, (i - exp_min + 1)] <-STis
    }
  } else {
    result_arrays_dims <- c(k, Y_dims[-1:-2])
    results_si <- array(dim = result_arrays_dims)
    results_sti <- array(dim = result_arrays_dims)

    N <- 2 ^ exp_min
    Sis <- apply(Y, margins_to_apply, function(x) calc_Si(x, N, k))
    STis <- apply(Y, margins_to_apply, function(x) calc_STi(x, N, k))
    results_si <- Sis
    results_sti <- STis

    N <- 2 ^ (exp_min + 1)
    Sis2 <- apply(Y, margins_to_apply, function(x) calc_Si(x, N, k))
    STis2 <- apply(Y, margins_to_apply, function(x) calc_STi(x, N, k))
    results_si <- abind::abind(results_si, Sis2, along = 0)
    results_sti <- abind::abind(results_sti, STis2, along = 0)

    if (length(sample_size) > 2) {
      for (i in (exp_min + 2):exp_max) {
        N <- 2 ^ i
        Sis <- apply(Y, margins_to_apply, function(x) calc_Si(x, N, k))
        STis <- apply(Y, margins_to_apply, function(x) calc_STi(x, N, k))
        results_si <- abind::abind(results_si, Sis, along = 1)
        results_sti <- abind::abind(results_sti, STis, along = 1)
      }
    }
    dimnames(results_si)[1] <- list("sample_size" = sample_size)
    dimnames(results_sti)[1] <- list("sample_size" = sample_size)
  }
  return(list(results_si,results_sti))
}

################################################################################################
################################################################################################


# Functions to calculate sensitivity indices

calc_Si <- function(y, N, k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat <- calc_Vhat(y, N)
  Ares <- y[1:N, 1]
  Bres <- y[1:N, 2]
  vis <- apply(y[1:N, 3:(k + 2)], 2, function(x) calc_Vi(x, N, Ares, Bres))
  si <- vis / vhat
  si # returns a matrix where rows are the k parameters, columns are the metrics used as model output
}

calc_Vi <- function(y, N, Ares, Bres) {
  vi <- (sum(Bres * (y - Ares))) / (N)
  vi
}


calc_STi <- function(y, N, k) {
  vhat <- calc_Vhat(y, N)
  Ares <- y[1:N, 1]
  vti <- apply(y[1:N, 3:(k + 2)], 2, function(x)
    calc_VTi(x, N, Ares))
  sti <- vti / vhat
  sti
}

calc_VTi <- function(y, N, Ares) {
  vti <- (sum((Ares - y) ^ 2)) / (2 * N)
  vti
}


calc_Vhat <- function(y, N) {
  fhat <- (sum(y[1:N, 1] + y[1:N, 2])) / (2 * N)
  vhat <- (sum((y[1:N, 1] - fhat) ^ 2 + (y[1:N, 2] - fhat) ^ 2)) / (2 *
                                                                      N - 1)
  vhat
}

################################################################################################
################################################################################################

#' Returns long form dataframe with results of SA and summary statistics of the run
#'
#' @param outs array with results from each simulation
#' @param SA array with first and total order indices for each target statistic requested
#' @param SA_conditions list of the SA Conditions established for the run
longFormThis <- function(outs, SA, SA_conditions, megadapt_conds) {
  # The table produced has the following columns (example):
  # input_parameter (budget)
  # community (Iztapalapa, All of Mexico City)
  # sample_size (8,16,32...)
  # outcome_name (first order sensitivity index, total order sensitivity index, min, max, mean)
  # target_statistic (mean vulnerability, etc)
  # value (some floating point value)

  Si_SA <- reshape2::melt(SA[[1]])
  Si_SA$outcome_name <- "first_order_sensitivity_index"
  STi_SA <- reshape2::melt(SA[[2]])
  STi_SA$outcome_name <- "total_order_sensitivity_index"
  long_SA <- rbind(Si_SA, STi_SA)
  names(long_SA)[1:5] <- c("sample_size","input_parameter","community","target_statistic","value")
  long_SA$outcome_name <- as.factor(long_SA$outcome_name)

  apply_margin <- 3:length(dim(outs))

  summary_outs <- apply(outs, apply_margin, function(x) appl_summary_statistics(x,megadapt_conds$out_stats))

  long_outs <- reshape2::melt(summary_outs)
  names(long_outs)[1] <- "outcome_name"

  long_outs$input_parameter <- "All"
  long_outs$sample_size <- 2^(SA_conditions$exp_max)

  long <- rbind(long_SA, long_outs)

  return(long)
}

appl_summary_statistics <- function(matr, summ_stats) {
  summary_out <- NULL
  for (stat in 1:length(summ_stats)) {
    summary_out[stat] <- match.fun(summ_stats[stat])(matr)
  }

  names(summary_out) <- summ_stats

  return(summary_out)
}

################################################################################################
################################################################################################
# Plots

#' Creates convergence plots, which should be used to confirm that the sample size chosen for the SA is big enough so the sensitivity estimators (Si and STi) converge to a certain number
#'
#' @export
#' @param results_table results table in long form
#' @param commun community (any of the 16 communities or "Global") from which to generate the convergence plots. Defaults to Global
#' @param output_vars vector of output variables of interest. Defaults to household patable water and sewer vulnerability
#' @param summary_stats summary statistics of interest, given as a vector of character strings. Defaults to mean.

plotSAConvergence <-function(results_table,
                             commun = "Global",
                             output_vars = c("household_potable_water_vulnerability", "household_sewer_vulnerability"),
                             summary_stats = c("mean"))

  {
    statistics <- paste(rep(output_vars, each = length(summary_stats)), summary_stats, sep = "_")

    if (!all(statistics %in% levels(results_table$target_statistic))) {
      stop("Requested output variables or summary statistics are not present in the results table")
    }
    if (length(commun) > 1) {
      stop("Please choose only one community")
    }

    number_metrics <- length(output_vars)

    first_commun_sens_indices <- subset(results_table, target_statistic %in% statistics & community == commun & outcome_name=="first_order_sensitivity_index")
    total_commun_sens_indices <- subset(results_table, target_statistic %in% statistics & community == commun & outcome_name=="total_order_sensitivity_index")

    first_commun_sens_indices$sample_size<-as.numeric(first_commun_sens_indices$sample_size)
    total_commun_sens_indices$sample_size<-as.numeric(total_commun_sens_indices$sample_size)

    first<-ggplot2::ggplot(first_commun_sens_indices, ggplot2::aes(sample_size,
                                                                   value,
                                                                   colour = input_parameter)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(cex = 0.7) +
      ggplot2::facet_wrap(~target_statistic, nrow = number_metrics, ncol = length(summary_stats), scales = "free") +
      ggplot2::scale_color_discrete(name="Input Parameters:") +
      ggplot2::ggtitle("Fist Order Sensitivity") +
      ggplot2::xlab("Sample Size") +
      ggplot2::ylab("Sensitivity Index") +
      ggplot2::theme_bw()+
      ggplot2::scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2)) +
      ggplot2::coord_cartesian(ylim = c(-0.15, 1.15))
      # ggplot2::theme(legend.position="top", legend.box = "horizontal")


    total<-ggplot2::ggplot(total_commun_sens_indices, ggplot2::aes(sample_size,
                                                                   value,
                                                                   colour = input_parameter)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(cex = 0.7) +
      ggplot2::facet_wrap(~target_statistic, nrow = number_metrics, ncol = length(summary_stats), scales = "free") +
      ggplot2::scale_color_discrete(name="Input Parameters:") +
      ggplot2::ggtitle("Total Order Sensitivity") +
      ggplot2::xlab("Sample Size") +
      ggplot2::ylab("Sensitivity Index") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0,1.2)) +
      ggplot2::coord_cartesian(ylim = c(-0.15, 1.15))

    ggplot2::ggsave(paste0("FirstConvergence_",commun,".png"), first, width = 20, height = (6 * number_metrics), units = "cm", dpi = 400)
    ggplot2::ggsave(paste0("TotalConvergence_",commun,".png"), total, width = 20, height = (6 * number_metrics), units = "cm", dpi = 400)

}

#' Creates heatmaps showing the sensitivity indices for each parameter-municipality combination
#'
#' @export
#' @param results_table results table in long form
#' @param output_var output variable of interest. Defaults to household patable water vulnerability
#' @param summary_stat summary statistic of interest. Defaults to mean.
#' @param at_sample_size sample size to get the data from. Defaults to the maximum available.

plotSAHeatmap <- function(results_table,
                          output_var = "household_potable_water_vulnerability",
                          summary_stat = "mean",
                          at_sample_size = max(results_table$sample_size))
  {
  if (length(output_var) > 1 | length(summary_stat) > 1) {
    stop("Please choose a single output variable and a single summary statistic")
  }
  statistic <- paste(output_var, summary_stat, sep = "_")

  first_statistic_sens_indices <- subset(results_table, target_statistic == statistic & sample_size == at_sample_size & outcome_name == "first_order_sensitivity_index")
  total_statistic_sens_indices <- subset(results_table, target_statistic == statistic & sample_size == at_sample_size & outcome_name == "total_order_sensitivity_index")

  f_heatmap <- ggplot2::ggplot(data = first_statistic_sens_indices, mapping = ggplot2::aes(input_parameter, community)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), color = "white", size = 0.1) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::scale_fill_gradient(low = "cadetblue3", high = "coral3", name = "Value", breaks = c(0.25,0.5,0.75,1)) +
    ggplot2::labs(x = "Input Parameters", y = NULL, title = "First Order Sensitivity Indices") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, vjust = 1))


  t_heatmap <- ggplot2::ggplot(data = total_statistic_sens_indices, mapping = ggplot2::aes(input_parameter, community)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), color = "white", size = 0.1) +
    ggplot2::scale_x_discrete(expand = c(0,0)) +
    ggplot2::scale_fill_gradient(low = "cadetblue3", high = "coral3", name = "Value", breaks = c(0.25,0.5,0.75,1)) +
    ggplot2::labs(x = "Input Parameters", y = NULL, title = "Total Order Sensitivity Indices") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, vjust = 1))

  ggplot2::ggsave(paste0("FirstHeatmap_",statistic,".png"), f_heatmap, width = 20, height = 15, units = "cm", dpi = 400)
  ggplot2::ggsave(paste0("TotalHeatmap_",statistic,".png"), t_heatmap, width = 20, height = 15, units = "cm", dpi = 400)

}

################################################################################################
################################################################################################
# Functions needed for condor runs
melt_with_info <- function (mats_array) {
  long_mat <- data.frame()

  rows <- dim(mats_array)[1]
  slices <- dim(mats_array)[3]
  id_count <- 1

  for (aslice in 1:slices) {
    for (arow in 1:rows) {
      row_info <- c(mats_array[arow,,aslice],arow, aslice, id_count)
      long_mat <- rbind(long_mat,row_info)
      id_count <- id_count + 1
    }
  }

  c_names <- c(dimnames(mats_array)[[2]],"row","slice","id")
  colnames(long_mat) <- c_names

  return(long_mat)
}
