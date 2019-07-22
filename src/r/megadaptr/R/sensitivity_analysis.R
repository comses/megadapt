#' Create possible input parameter combinations for running the sensitivity analysis
#'
#' @param SAConds sensitivity analysis configuration object
#' @param SAParams sensitivity analysis input parameter sample space
#' @return input parameter combinations as an N-dimensional array
createLinearMatrices <- function(SAConds, SAParams) {
  N <- 2 ^ SAConds$exp.max
  k <- length(SAParams)

  interc <- rep(0, k)
  for (i in 1:k) {
    interc[i] <- SAParams[[i]]$min
  }

  slope <- rep(0, k)
  for (j in 1:k) {
    slope[j] <- (SAParams[[j]]$max - SAParams[[j]]$min)
  }


  S = qrng::sobol(N + 4, 2 * k, skip = 1)

  for (i in 1:(2 * k)) {
    if (i <= k) {
      S[, i] <- S[, i] * slope[i] + interc[i]
      if (SAParams[[i]]$isInteger == T) {
        S[, i] = round(S[, i])
      }
    } else {
      S[, i] = S[, i] * slope[i - k] + interc[i - k]
      if (SAParams[[i - k]]$isInteger == T) {
        S[, i] = round(S[, i])
      }
    }
  }


  ABMats <- array(dim = c(N, k, (k + 2)))
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

  # ABpath<-paste("/VBSAMatrices/ABMats",as.character(N),".rds",sep="")

  # saveRDS(ABMats,ABpath)

}

################################################################################################
################################################################################################

#'Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.
#'
#' @param SAConditions sensivity analysis configuration object
#' @param SAParams sensitivity analysis input parameter sample space
#' @param oMetricNames outcomes variable name vector
#' @param ABMats scaled sobol matrix for samples of the input space
VBSA <- function(SAConditions, SAParams, oMetricNames) {

  ABMats <- createLinearMatrices(SAConditions, SAParams)

  Y <- runSimulations(SAConditions, SAParams, ABMats)

  resultss <- calcSensitivityIndices(SAConditions, SAParams, Y)

  outt <- longFormThis(outs = Y, SA = resultss, SAConditions = SAConditions)

  return(outt)
}

################################################################################################
################################################################################################

runSimulations <- function (SAConditions, SAParams, ABMats) {

  if (SAConditions$onCluster) {
    future::plan(future::multisession)
  } else {
    future::plan(future::sequential)
  }

  exp.min <- SAConditions$exp.min
  exp.max <- SAConditions$exp.max
  maxN <- 2 ^ exp.max
  k <- length(SAParams)
  noStats <- length(SAConditions$outStats)
  communities <- SAConditions$communities
  oMetricNames <- SAConditions$oMetricNames
  useThisFunction <- match.fun(SAConditions$whichmodel)

  if (runMod) {
    sample_number <- as.character(1:maxN)
    AB_matrices_numbers <- paste("AB", 1:k, sep="")
    matrix_names <- c("A","B", AB_matrices_numbers)

    #Create array of results: number of rows corresponds to number of outcome variables, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
    Yi <- array(dim = c(length(oMetricNames), maxN, (dim(ABMats)[3])),
                dimnames = list( oMetricNames,
                                 sample_number,
                                 matrix_names))
    if (SAConditions$municip) {
      # 2 dim now is 17 (number of mun + total), shifted the rest dims
      Yi <-
        array(dim = c(length(oMetricNames) * noStats,
                      length(communities),
                      maxN,
                      (dim(ABMats)[3])),
              dimnames = list(NULL,
                              communities,
                              sample_number,
                              matrix_names))
    }

    #Create megadapt object
    megadapt <- megadapt_create(params = params_create(n_steps = SAConditions$simyears))

    param_names <- NULL
    for (pname in 1:length(SAParams)) {
      param_names[pname] <- SAParams[[pname]]$name
    }

    # Yi <- future.apply::future_apply(ABMats, c(1, 3), function(x) useThisFunction(x, megadapt, SAConditions, oMetricNames, param_names))

    Yi <- apply(ABMats, c(1, 3), function(x) useThisFunction(x, megadapt, SAConditions, oMetricNames, param_names))


    if (SAConditions$municip) {
      Y <- array(unlist(Yi),
                 dim = c(
                   length(communities),
                   length(oMetricNames) * noStats,
                   maxN,
                   (dim(ABMats)[3])),
                 dimnames = list("community" = communities,
                                 "target_statistic" = colnames(Yi[[1]]),
                                 "number_of_sample" = sample_number,
                                 "matrix_name" = matrix_names))
      Y <- aperm(Y, c(3, 4, 1, 2))
      saveRDS(Y, "modelOuts")

    } else if (length(oMetricNames) > 1) {
      Y <- aperm(Yi, c(2, 3, 1))
      dimnames(Y) <- list("number_of_sample" = sample_number,
                          "matrix_name" = matrix_names,
                          "target_statistic" = oMetricNames)

      saveRDS(Y, "modelOuts")
    } else {
      Y <- Yi
    }

  } else {
    Y <- readRDS("modelOuts")
  }

  return(Y)
}

################################################################################################
################################################################################################

# Function that gets the summary statistic from the variables of interest
modelMetrics <- function(x, megadapt, SAConditions, oMetricNames, param_names) {
  #Assign values to variables
  new_params <- assign_values_param_names(x, param_names, SAConditions)

  new_p_list <- do.call(params_create,new_params)

  # Add parameters to the megadapt object
  megadapt <- modify_megadapt_params(megadapt, new_p_list)

  #Run model and get results from last year
  results <- simulate(megadapt)

  lastT <- max(results$year)

  summary_funcs <- as.list(SAConditions$outStats)
  names(summary_funcs) <- SAConditions$outStats

  if (SAConditions$municip) {
    Vlast <-
      subset(results, year == lastT, select = c("geographic_id", oMetricNames))
    Vlast$Mun <-
      substr(Vlast$geographic_id, start = 1, stop = 5)

    metrics <- dplyr::group_by(Vlast, Mun) %>% dplyr::summarise_at(oMetricNames, summary_funcs, na.rm=TRUE) %>%
      dplyr::select(-Mun) %>% dplyr::ungroup()
    total<-dplyr::summarise_at(Vlast, oMetricNames, summary_funcs, na.rm=TRUE)
    metrics<-rbind(metrics,total)

  } else {
    Vlast <- subset(results, year == lastT, select = oMetricNames)
    metrics <- apply(Vlast, 2, function(x)
      mean(x, na.rm = T))
  }

  return(metrics)
}

# Toy function to check everything else about SA makes sense
toyFunction <- function(x, megadapt, SAConditions, oMetricNames, param_names) {
  Vlast <- matrix(c(x, x / 2), ncol = 2)
  means <- apply(Vlast, 2, function(x) mean(x, na.rm = T))
  return(means)
}

# Example from book chapter to confirm we are getting same behavior (not getting exactly same values, maybe due to the use of a different Sobol generator)
bookEx <- function(x, megadapt, SAConditions, oMetricNames, param_names) {
  y = 3 * (x[1] ^ 2) + 2 * (x[1] * x[2]) - 2 * x[3]
  return(y)
}

################################################################################################
################################################################################################

assign_values_param_names <- function(values, pnames, SAConditions) {

  custom_params <- list()
  param_ind <- 1
  for (param_name in pnames) {
    if (!is.na(values[param_ind])) {
      custom_params[[param_name]] <- values[param_ind]
    }
    param_ind <- param_ind + 1
  }
  custom_params$n_steps <- SAConditions$simyears

  return(custom_params)
}

modify_megadapt_params <- function (megadapt,
                                    params,
                                    sacmex_fnss_creator = sacmex_seperate_action_budgets_fnss_create) {
  assert_shape(
    params,
    shape = list(
      new_infrastructure_effectiveness_rate = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      maintenance_effectiveness_rate = function(x) checkmate::check_numeric(x, lower = 0, upper = 1),
      n_steps = function(x) checkmate::check_int(x, lower = 0),
      infrastructure_decay_rate = function(x) checkmate::check_numeric(0.01, lower = 0, upper = 1),
      budget = function(x) checkmate::check_int(x, lower = 0),
      half_sensitivity_ab = function(x) checkmate::check_int(x, lower = 1, upper = 20),
      half_sensitivity_d = function(x) checkmate::check_int(x, lower = 1, upper = 20),
      climate_scenario = function(x) checkmate::check_int(x, lower = 1, upper = 12)
    ))

  if (is.character(sacmex_fnss_creator)) {
    checkmate::assert_choice(sacmex_fnss_creator, choices = c('sacmex_seperate_action_budgets_fnss_create', 'sacmex_fnss_create'))
    sacmex_fnss_creator <- get(sacmex_fnss_creator)
  }
  mental_models <- mental_model_constant_strategies()

  value_function_config <- value_function_config_default()
  megadapt$climate_fnss <- climate_fnss_create(params$climate_scenario)
  megadapt$resident_fnss = resident_fnss_create(
    value_function_config = value_function_config,
    mental_model_strategy = mental_models$resident_limit_strategy,
    half_sensitivity_ab = params$half_sensitivity_ab,
    half_sensitivity_d = params$half_sensitivity_d
  )
  megadapt$sacmex_fnss = sacmex_fnss_creator(
    value_function_config = value_function_config,
    sewer_mental_model_strategy = mental_models$sewer_water_sacmex_limit_strategy,
    potable_water_mental_model_strategy = mental_models$potable_water_sacmex_limit_strategy,
    params = params,
    potable_water_budget = params$budget,
    sewer_budget = params$budget,
    flooding_fnss = megadapt$flooding_fnss,
    ponding_fnss = megadapt$ponding_fnss
  )
  megadapt$water_scarcity_fnss = water_scarcity_index_fnss_create(value_function_config = value_function_config)

  return(megadapt)
}


#' Modify an existing megadapt model
#'
#' @export
#' @param model megadapt model
#' @param params parameters to override from prototype model
modify_megadapt_model <- function(model, params) {
  model$params <- do.call(create_params, params)
  model
}

################################################################################################
################################################################################################

calcSensitivityIndices <- function(SAConditions, SAParams, Y) {

  exp.min <- SAConditions$exp.min
  exp.max <- SAConditions$exp.max
  maxN <- 2 ^ exp.max
  k <- length(SAParams)
  noStats <- length(SAConditions$outStats)
  communities <- SAConditions$communities
  oMetricNames <- SAConditions$oMetricNames


  param <- NULL
  for (i in 1:length(SAParams)) {
    param[i] <- SAParams[[i]]$name
  }
  sample_size <- as.character(2^(exp.min:exp.max))

  if (SAConditions$municip) {
    #rows<->parameters, columns<->N, third<->Si or STi, fourth <-> communities, fifth <-> outcome variables
    resultss <-
      array(dim = c(k,
                    (exp.max - exp.min + 1),
                    2,
                    length(communities),
                    length(dimnames(Y)[[4]])),
            dimnames = list("input_parameter" = param,
                            "sample_size" = sample_size,
                            "outcome_name" = c("first_order_sensitivity_i","total_order_sensitivity_i"),
                            "community" = communities,
                            "target_statistic" = dimnames(Y)[[4]]))
  } else {
    #rows<->parameters, columns<->N, third<->Si or STi, fourth <-> outcome variables
    resultss <-
      array(dim = c(k,
                    (exp.max - exp.min + 1),
                    2,
                    length(oMetricNames)),
            dimnames = list("input_parameter" = param,
                            "sample_size" = sample_size,
                            "outcome_name" = c("first_order_sensitivity_i","total_order_sensitivity_i"),
                            "target_statistic" = oMetricNames))
  }



  if (length(oMetricNames) == 1 && SAConditions$municip == FALSE) {
    for (i in exp.min:exp.max) {
      N <- 2 ^ i

      Sis <- calc.Si(Y, N, k)
      STis <- calc.STi(Y, N, k)
      resultss[1:k, (i - exp.min + 1), ] <- c(Sis, STis)
    }
  } else if (SAConditions$municip)  {
    for (i in exp.min:exp.max) {
      N <- 2 ^ i
      Sis <- apply(Y, c(3, 4), function(x)
        calc.Si(x, N, k))
      # print(Sis)
      STis <- apply(Y, c(3, 4), function(x)
        calc.STi(x, N, k))
      resultss[1:k, (i - exp.min + 1), 1, 1:length(communities), 1:length(dimnames(Y)[[4]])] <- Sis
      resultss[1:k, (i - exp.min + 1), 2, 1:length(communities), 1:length(dimnames(Y)[[4]])] <- STis
    }
  } else {
    for (i in exp.min:exp.max) {
      N <- 2 ^ i
      Sis <- apply(Y, c(3), function(x) calc.Si(x, N, k))
      # print(Sis)
      STis <- apply(Y, c(3), function(x) calc.STi(x, N, k))
      resultss[1:k, (i - exp.min + 1), 1, 1:length(oMetricNames)] <- Sis
      resultss[1:k, (i - exp.min + 1), 2, 1:length(oMetricNames)] <- STis
    }
  }
  return(resultss)
}

################################################################################################
################################################################################################


# Functions to calculate sensitivity indices

calc.Si <- function(y, N, k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat <- calc.Vhat(y, N)
  # print(vhat)
  Ares <- y[1:N, 1]
  Bres <- y[1:N, 2]
  vis <- apply(y[1:N, 3:(k + 2)], 2, function(x)
    calc.Vi(x, N, Ares, Bres))
  # print(vis)
  si <- vis / vhat
  si # returns a matrix where rows are the k parameters, columns are the metrics used as model output
}

calc.Vi <- function(y, N, Ares, Bres) {
  vi <- (sum(Bres * (y - Ares))) / (N)
  vi
}


calc.STi <- function(y, N, k) {
  vhat <- calc.Vhat(y, N)
  Ares <- y[1:N, 1]
  vti <- apply(y[1:N, 3:(k + 2)], 2, function(x)
    calc.VTi(x, N, Ares))
  sti <- vti / vhat
  sti
}

calc.VTi <- function(y, N, Ares) {
  vti <- (sum((Ares - y) ^ 2)) / (2 * N)
  vti
}


calc.Vhat <- function(y, N) {
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
#' @param SAConditions list of the SA Conditions established for the run
longFormThis <- function(outs, SA, SAConditions) {
  # The table produced has the following columns (example):
  # input_parameter (budget)
  # community (Iztapalapa, All of Mexico City)
  # sample_size (8,16,32...)
  # outcome_name (first order sensitivity index, total order sensitivity index, min, max, mean)
  # target_statistic (mean vulnerability, etc)
  # value (some floating point value)

  longSA <- reshape2::melt(SA)
  # longSA <- dplyr::select(longSA, -sample_size)

  apply_margin <- 3:length(dim(outs))

  summaryOuts <- apply(outs, apply_margin, appl_summary_statistics)

  if (SAConditions$municip) {
    dimnames(summaryOuts) <- list("outcome_name" = SAConditions$outStats,
                                  "community" = dimnames(outs)$community,
                                  "target_statistic" = dimnames(outs)$target_statistic)
  } else {
    dimnames(summaryOuts) <- list("outcome_name" = SAConditions$outStats,
                                  "target_statistic" = oMetricNames)
  }

  longOuts <- reshape2::melt(summaryOuts)

  longOuts$input_parameter <- "All"
  longOuts$sample_size <- 2^(SAConditions$exp.max)

  long <- rbind(longSA, longOuts)


  return(long)
}

appl_summary_statistics <- function(matr) {
  meann <- mean(matr)
  maxx <- max(matr)
  minn <- min(matr)

  return(c(meann, maxx, minn))
}

################################################################################################
################################################################################################
# Plots

#' Creates convergence plots, which should be used to confirm that the sample size chosen for the SA is big enough so the sensitivity estimators (Si and STi) converge to a certain number
#'
#' @param results_table results table in long form
#' @param commun community (any of the 16 communities or "Global") from which to generate the convergence plots. Defaults to Global
#' @param summ_stat summary statistic(s) from which to generate the convergence plots. Allows to reduce the number of subplots. Defaults to all the statistics in SAConditions$outStats
plotSAConvergence <-
  function(
    results_table,
    commun = "Global",
    summ_stat = levels(results_table$outcome_name)[-c(1,2)],
    logScale=F)
  {
    sens_inidices <- subset(results_table, outcome_name=="first_order_sensitivity_i" | outcome_name=="total_order_sensitivity_i")
    Ns <- unique(sens_inidices$sample_size)
    avail_stats <- levels(results_table$outcome_name)[-c(1,2)]

    if (!all(summ_stat %in% avail_stats)) {
      stop("Summary statistic(s) were not used in this SA")
    }

    number_metrics <- length(levels(results_table$target_statistic)) / length(avail_stats)
    # par(mfrow = c(number_metrics,length(summ_stat)))

    commun_sens_indices <- subset(sens_inidices, community == commun)

    first_commun_sens_indices <- subset(commun_sens_indices, outcome_name=="first_order_sensitivity_i")
    total_commun_sens_indices <- subset(commun_sens_indices, outcome_name=="total_order_sensitivity_i")

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

    if (logScale) {
      first <- first + ggplot2::scale_x_continuous(trans = "log2")
    }

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

    if (logScale) {
      total <- total + ggplot2::scale_x_continuous(trans = "log2")
    }


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
  #   Ns <- seq(SAConditions$exp.min, SAConditions$exp.max)
  #   numberPlots <- SAConditions$oMetricNames
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

plotPieCharts <- function(x, expon, metric) {
  n <- expon - SAConditions$exp.min + 1
  if (n < 1) {
    stop("Exponent has to be equal or greater than exp.min")
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





