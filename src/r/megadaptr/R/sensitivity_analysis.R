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
      if (SA_params[[i]]$isInteger == T) {
        S[, i] = round(S[, i])
      }
    } else {
      S[, i] = S[, i] * slope[i - k] + interc[i - k]
      if (SA_params[[i - k]]$isInteger == T) {
        S[, i] = round(S[, i])
      }
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
  # ABMats[,,2] <- S[5:(N+4),(k+1):(2*k)] #Pairing A with B taken from 4 rows below (p.38 from book chapter)
  # for (j in 1:k) {
  #   ABMats[,j,(j+2)] <- S[5:(N+4),j+k]
  # }

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
                       SA_conditions = SA_conditions)

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
      # out_metrics <- list(metrics = metrics,
      #                     muns = muns)

    } else {
      Vlast <- subset(results, year == lastT, select = out_metric_names)
      metrics <- apply(Vlast, 2, function(x) mean(x, na.rm = T))
      metrics_a <- metrics
      # out_metrics <- list(metrics = metrics,
      #                     muns = NULL)
    }

    rreturn <- list(metrics = metrics_a,
                    dims = dim(metrics_a))
    return(rreturn)
  }


}

one_megadapt_superficial_params_simulator <- function(params, megadapt_conds) {

  # megadapt_conds <- list(
  #   sim_years = 4,
  #   municip = T,
  #   out_stats = c("mean","max","min"),
  #   out_metric_names = c("household_potable_water_vulnerability", "household_sewer_vulnerability", "flooding_index",
  #                        "ponding_index","scarcity_index_exposure", "scarcity_index_sensitivity")
  # )

  params$n_steps <- megadapt_conds$sim_years

  params_list <- do.call(params_create, params)

  megadapt <- megadapt_create(params = params_list)

  # param_names <- NULL
  # for (pname in 1:length(SA_params)) {
  #   param_names[pname] <- SA_params[[pname]]$name
  # }

  summary_funcs <- as.list(megadapt_conds$out_stats)
  names(summary_funcs) <- megadapt_conds$out_stats
  out_metric_names <- megadapt_conds$out_metric_names

  results <- simulate(megadapt)

  lastT <- max(results$year)

  Vlast <- subset(results, year == lastT, select = c("geographic_id", out_metric_names))
  Vlast$Mun <- substr(Vlast$geographic_id, start = 1, stop = 5)

  metrics <- dplyr::group_by(Vlast, Mun) %>% dplyr::summarise_at(out_metric_names, summary_funcs, na.rm=TRUE)
  total <- dplyr::summarise_at(Vlast, out_metric_names, summary_funcs, na.rm=TRUE)
  total$Mun <- "Global"
  metrics <- rbind(metrics,total)
  # muns <- metrics$Mun
  muns <- community_names()
  metrics <- dplyr::select(metrics, -Mun)
  # metrics_a <- simplify2array(metrics)
  # dimnames(metrics_a) <- list("communities" = muns,
                              # "target_statistic" = colnames(metrics))
  metrics_b <- as.data.frame(metrics)
  metrics_b$community <- muns

  return(metrics_b)

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

assign_community_name <- function (comm_number) {
  muns <- list("1" = "Azcapotzalco",
            "2" = "Coyoacan",
            "3" = "Cuajimalpa_de_Morelos",
            "4" = "Gustavo_A_Madero",
            "5" = "Iztacalco",
            "6" = "Iztapalapa",
            "7" = "La_Magdalena_Contreras",
            "8" = "Milpa_Alta",
            "9" = "Alvaro_Obregon",
            "10" = "Tlahuac",
            "11" = "Tlalpan",
            "12" = "Xochimilco",
            "13" = "Benito_Juarez",
            "14" = "Cuauhtemoc",
            "15" = "Miguel_Hidalgo",
            "16" = "Venustiano_Carranza",
            "17" = "Global")

  return(switch(as.character(comm_number), muns))
}

################################################################################################
################################################################################################

calcSensitivityIndices <- function(SA_conditions, SA_params, Y) {

  exp_min <- SA_conditions$exp_min
  exp_max <- SA_conditions$exp_max
  maxN <- 2 ^ exp_max
  k <- length(SA_params)
  # noStats <- length(SA_conditions$outStats)
  # communities <- SA_conditions$communities
  # oMetricNames <- SA_conditions$oMetricNames


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
        # print(Sis)
        STis <- apply(Y, margins_to_apply, function(x) calc_STi(x, N, k))
        # resultss[1:k, (i - exp_min + 1), 1, 1:length(communities), 1:length(dimnames(Y)[[4]])] <- Sis
        # resultss[1:k, (i - exp_min + 1), 2, 1:length(communities), 1:length(dimnames(Y)[[4]])] <- STis
        results_si <- abind::abind(results_si, Sis, along = 1)
        results_sti <- abind::abind(results_sti, STis, along = 1)
        # results_si[1:k, (i - exp_min + 1),,] <- Sis
        # results_sti[1:k, (i - exp_min + 1),,] <- STis
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
longFormThis <- function(outs, SA, SA_conditions) {
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
  # longSA <- dplyr::select(longSA, -sample_size)

  apply_margin <- 3:length(dim(outs))

  summary_outs <- apply(outs, apply_margin, function(x) appl_summary_statistics(x,SA_conditions$summary_stats))

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
#' @param results_table results table in long form
#' @param commun community (any of the 16 communities or "Global") from which to generate the convergence plots. Defaults to Global

plotSAConvergence <-function(results_table,
                             commun = "Global")

  {
    sens_inidices <- subset(results_table, outcome_name=="first_order_sensitivity_index" | outcome_name=="total_order_sensitivity_index")
    Ns <- unique(sens_inidices$sample_size)
    summ_stat <- levels(results_table$outcome_name)[order(levels(results_table$outcome_name))][-c(1,5)]
    number_metrics <- length(levels(results_table$target_statistic)) / length(summ_stat)
    # par(mfrow = c(number_metrics,length(summ_stat)))

    commun_sens_indices <- subset(sens_inidices, community == commun)

    first_commun_sens_indices <- subset(commun_sens_indices, outcome_name=="first_order_sensitivity_index")
    total_commun_sens_indices <- subset(commun_sens_indices, outcome_name=="total_order_sensitivity_index")

    first_commun_sens_indices$sample_size<-as.numeric(first_commun_sens_indices$sample_size)
    total_commun_sens_indices$sample_size<-as.numeric(total_commun_sens_indices$sample_size)

    first<-ggplot2::ggplot(first_commun_sens_indices, ggplot2::aes(sample_size,
                                                                   value,
                                                                   colour = input_parameter)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(cex = 0.7) +
      ggplot2::facet_wrap(~target_statistic, ncol = number_metrics, nrow = length(summ_stat), scales = "free") +
      ggplot2::scale_color_discrete(name="Input Parameter") +
      ggplot2::ggtitle("Fist Order Sensitivity")+
      ggplot2::xlab("Sample Size")+
      ggplot2::ylab("Sensitivity Index")+
      ggplot2::theme_bw()+
      ggplot2::scale_y_continuous(limits = c(-0.05, 1.05))


    total<-ggplot2::ggplot(total_commun_sens_indices, ggplot2::aes(sample_size,
                                                                   value,
                                                                   colour = input_parameter)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(cex = 0.7) +
      ggplot2::facet_wrap(~target_statistic, ncol = number_metrics, nrow = length(summ_stat), scales = "free") +
      ggplot2::scale_color_discrete(name="Input Parameter") +
      ggplot2::ggtitle("Total Order Sensitivity")+
      ggplot2::xlab("Sample Size")+
      ggplot2::ylab("Sensitivity Index")+
      ggplot2::theme_bw()+
      ggplot2::scale_y_continuous(limits = c(-0.05, 1.05))

    first
    dev.new()
    total

    # for (eachstat in unique(first_commun_sens_indices$target_statistic)) {
    #   # ggplot2::ggplot(first_commun_sens_indices, aes(sample_size, value, colour = input_parameter)) +
    #   #   geom_line()
    #   plot(first_commun_sens_indices$sample_size[first_commun_sens_indices$target_statistic==eachstat], first_commun_sens_indices$value[first_commun_sens_indices$target_statistic==eachstat],xlim=c(0,5),ylim=c(0,1))
    # }

    # dev.new()
    # for (eachstat in total_commun_sens_indices$target_statistic) {
    #   ggplot2::ggplot(total_commun_sens_indices, aes(sample_size, value, colour = input_parameter)) +
    #     geom_line()
    # }


  }



  #
  #   #Plots both Si and STi vs N (Maximum of 5 metrics)
  #   Ns <- seq(SA_conditions$exp_min, SA_conditions$exp_max)
  #   numberPlots <- SA_conditions$oMetricNames
  #   par(mfrow = c(ceiling(numberPlots / 2), 2))
  #   paramcolors <-
  #     c(
  #       "darkcyan",
  #       "blue4",
  #       "darkorchid",
  #       "darkorange2",
  #       "brown2",
  #       "chartreuse3",
  #       "skyblue",
  #       "orange",
  #       "magenta"
  #     )
  #   for (i in 1:numberPlots) {
  #     withinThisRange <- range(x[, , i])
  #     plot(
  #       Ns,
  #       x[1, , i],
  #       ylim = c(withinThisRange[1], withinThisRange[2] + 0.05),
  #       col = paramcolors[1],
  #       xlab = "N",
  #       ylab = ""
  #     )
  #     if (i <= (numberPlots / 2)) {
  #       title(main = bquote(.(oMetricNames[i]) ~ S[i]),
  #             ylab = bquote(S[i]))
  #     } else {
  #       title(main = bquote(.(oMetricNames[i - (numberPlots / 2)]) ~ S[Ti]),
  #             ylab = bquote(S[Ti]))
  #     }
  #     for (j in 2:dim(x)[1]) {
  #       points(Ns, x[j, , i], col = paramcolors[j])
  #     }
  #     legendnames <- paste("x", seq(1:dim(x)[1]))
  #     legend(
  #       "right",
  #       legendnames,
  #       fill = paramcolors[1:(dim(x)[1])],
  #       border = "white",
  #       bty = "n"
  #     )
  #   }
  # }

plotPieCharts <- function(x, expon, metric)
  {
  n <- expon - SA_conditions$exp_min + 1
  if (n < 1) {
    stop("Exponent has to be equal or greater than exp_min")
  }
  metrOfInterest <- match(metric, oMetricNames)
  jump <- (dim(x)[3]) / 2
  sivals <- abs(x[, n, metrOfInterest])
  siperc <- round((sivals / sum(sivals)) * 100)
  silabl <- paste(siperc, " %")
  #add names of parameters
  stivals <- abs(x[, n, (metrOfInterest + jump)])
  stiperc <- round((stivals / sum(stivals)) * 100)
  stilabl <- paste(stiperc, " %")
  #same here
  par(mfrow = c(1, 2))
  paramcolors <-
    c(
      "darkcyan",
      "blue4",
      "darkorchid",
      "darkorange2",
      "brown2",
      "chartreuse3",
      "skyblue",
      "orange",
      "magenta"
    )
  # if (length(sivals) <= length(paramcolors)) {
  #   piecolors <- paramcolors[1:length(sivals)]
  # } else {
  #   piecolors <- rainbow(length(sivals))
  # }
  pie(
    sivals,
    main = bquote(S[i]),
    labels = silabl,
    col = topo.colors(length(sivals))
  )
  pie(
    stivals,
    main = bquote(S[Ti]),
    labels = stilabl,
    col = topo.colors(length(sivals))
  )
  #To do: add percentages and labels
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
#
# assign_parameter_name_from_slice <- function(slice_num, SA_params) {
#   param_num <- slice_num - 2
#   param_name <- SA_params[[param_num]]$name
#   return(param_name)
# }




