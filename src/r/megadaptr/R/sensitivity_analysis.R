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

#(Originally in file VBSAmain.R)
#Running the model and retrieving the variables of interest

runn <- function(megadapt, oMetricNames) {
  # set.seed(1000)
  results <- simulate_megadapt(megadapt)

  lastT <- max(results$time_sim)
  Vlast <- subset(results, time_sim == lastT, select = oMetricNames)
  return(Vlast)
}


################################################################################################
################################################################################################

#(Originally in file VBSA.R)

#'Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.
#'
#' @param SAConditions sensivity analysis configuration object
#' @param SAParams sensitivity analysis input parameter sample space
#' @param oMetricNames outcomes variable name vector
VBSA <- function(SAConditions, SAParams, oMetricNames,ABMats) {
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
  #
  # #Create matrix
  # ABMats<-createLinearMatrices(maxN,k)

  # #Names of matrices change according to N
  # ABname<-paste("ABMats",as.character(maxN),".rds",sep="")
  # #Assign files to A and B matrices and ABi array
  # ABMats<-readRDS(paste("VBSAMatrices/",ABname,sep=""))


  #Create array of results: number of rows corresponds to number of model metrics, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
  # 2 dim now is 17 (number of mun + total), shifted the rest dims

  if (runMod) {
    sample_number <- as.character(1:maxN)
    AB_matrices_numbers <- paste("AB", 1:k, sep="")
    matrix_names <- c("A","B", AB_matrices_numbers)

    Yi <- array(dim = c(length(oMetricNames), maxN, (dim(ABMats)[3])),
                dimnames = list( oMetricNames,
                                 sample_number,
                                 matrix_names))

    #Run simulations

    if (SAConditions$whichmodel == "custom") {
      if (SAConditions$municip) {

        communities <- c(
          "Azcapotzalco",
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
          "Global"
        )

        Yi <-
          array(dim = c(length(oMetricNames) * noStats,
                        SAConditions$noMunip + 1,
                        maxN,
                        (dim(ABMats)[3])),
                dimnames = list(NULL,
                                communities,
                                sample_number,
                                matrix_names))
      }
      megadapt <-
        build_megadapt_model(
          data_root_dir = data_root_dir,
          mental_model_file_names = mental_model_file_names,
          params = list(n_steps = SAConditions$simyears)
        )


      Yi <-
        future.apply::future_apply(ABMats, c(1, 3), function(x)
          modelMetrics(x, megadapt, SAConditions, oMetricNames))


    } else if (SAConditions$whichmodel == "toy") {
      Yi <- future.apply::future_apply(ABMats, c(1, 3), toyFunction)
    } else if (SAConditions$whichmodel == "book") {
      Yi <- future.apply::future_apply(ABMats, c(1, 3), bookEx)
    }

    if (SAConditions$municip) {
      Y <- array(unlist(Yi),
              dim = c(
                SAConditions$noMunip + 1,
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



  #Calculate sensitivity indices
  #rows<->parameters, columns<->N, third<->(output metrics)*2: first Si for all metrics, then STi for all metrics
  #outdated
  noStats <- length(SAConditions$outStats)
  param <- NULL
  for (i in 1:length(SAParams)) {
    param[i] <- SAParams[[i]]$name
  }
  sample_size <- as.character(seq(2,maxN,2))

  if (SAConditions$municip) {
    resultss <-
      array(dim = c(k,
                    (exp.max - exp.min + 1),
                    2,
                    (SAConditions$noMunip + 1),
                    length(dimnames(Y)[[4]])),
            dimnames = list("input_parameter" = param,
                            "sample_size" = sample_size,
                            "outcome_name" = c("first_order_sensitivity_i","total_order_sensitivity_i"),
                            "community" = communities,
                            "target_statistic" = dimnames(Y)[[4]]))
  } else {
    resultss <-
      array(dim = c(k, (exp.max - exp.min + 1), 2, length(oMetricNames)),
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
      print(Sis)
      STis <- apply(Y, c(3, 4), function(x)
        calc.STi(x, N, k))
      resultss[1:k, (i - exp.min + 1), 1, 1:(SAConditions$noMunip + 1), 1:length(dimnames(Y)[[4]])] <- Sis
      resultss[1:k, (i - exp.min + 1), 2, 1:(SAConditions$noMunip + 1), 1:length(dimnames(Y)[[4]])] <- STis
    }
  } else {
    for (i in exp.min:exp.max) {
      N <- 2 ^ i
      Sis <- apply(Y, c(3), function(x) calc.Si(x, N, k))
      print(Sis)
      STis <- apply(Y, c(3), function(x) calc.STi(x, N, k))
      resultss[1:k, (i - exp.min + 1), 1, 1:length(oMetricNames)] <- Sis
      resultss[1:k, (i - exp.min + 1), 2, 1:length(oMetricNames)] <- STis
    }

  }

  outt <- longFormThis(outs = Y, SA = resultss, SAConditions = SAConditions)

  return(outt)
}

# Function that gets the summary statistic from the variables of interest
modelMetrics <- function(x, megadapt, SAConditions, oMetricNames) {
  #Assign values to variables
  custom_params <- list()
  param_ind <- 1
  for (param_name in c(
    'new_infrastructure_effectiveness_rate',
    'maintenance_effectiveness_rate',
    'infrastructure_decay_rate',
    'budget'
  )) {
    if (!is.na(x[param_ind])) {
      custom_params[[param_name]] <- x[param_ind]
    }
    param_ind <- param_ind + 1
  }
  custom_params$n_steps <- SAConditions$simyears



  # # Add parameters to the megadapt object
  megadapt <- modify_megadapt_model(megadapt, custom_params)

  # print(megadapt$params)

  #Run model and get results from last year
  results <- simulate_megadapt(megadapt)

  lastT <- max(results$time_sim)

  if (SAConditions$municip) {
    Vlast <-
      subset(results, time_sim == lastT, select = c("cvgeo", oMetricNames))
    Vlast$Mun <-
      substr(Vlast$cvgeo, start = 1, stop = 5)

    metrics <- dplyr::group_by(Vlast, Mun) %>% dplyr::summarise_at(oMetricNames, dplyr::funs(mean,max,min), na.rm=TRUE) %>%
      dplyr::select(-Mun) %>% dplyr::ungroup()
    total<-dplyr::summarise_at(Vlast, oMetricNames, dplyr::funs(mean,max,min), na.rm=TRUE)
    metrics<-dplyr::full_join(metrics, total)

  } else {
    Vlast <- subset(results, time_sim == lastT, select = oMetricNames)
    metrics <- apply(Vlast, 2, function(x)
      mean(x, na.rm = T))
  }

  return(metrics)
}




################################################################################################
################################################################################################
#(Originally in file VBSA.R)

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

# Toy function to check everything else about SA makes sense
toyFunction <- function(x) {
  Vlast <- matrix(c(x, x / 2), ncol = 2)
  means <- apply(Vlast, 2, function(x)
    mean(x, na.rm = T))
}

# Example from book chapter to confirm we are getting same values.
bookEx <- function(x) {
  y = 3 * (x[1] ^ 2) + 2 * (x[1] * x[2]) - 2 * x[3]
}

################################################################################################
################################################################################################
# Plots

plotConvergence <-
  function(x) {
    #Plots both Si and STi vs N (Maximum of 5 metrics)
    Ns <- seq(SAConditions$exp.min, SAConditions$exp.max)
    numberPlots <- dim(x)[3]
    par(mfrow = c((numberPlots / 2), 2))
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
    for (i in 1:numberPlots) {
      thisRange <- range(x[, , i])
      plot(
        Ns,
        x[1, , i],
        ylim = c(thisRange[1], thisRange[2] + 0.05),
        col = paramcolors[1],
        xlab = "N",
        ylab = ""
      )
      if (i <= (numberPlots / 2)) {
        title(main = bquote(.(oMetricNames[i]) ~ S[i]),
              ylab = bquote(S[i]))
      } else {
        title(main = bquote(.(oMetricNames[i - (numberPlots / 2)]) ~ S[Ti]),
              ylab = bquote(S[Ti]))
      }
      for (j in 2:dim(x)[1]) {
        points(Ns, x[j, , i], col = paramcolors[j])
      }
      legendnames <- paste("x", seq(1:dim(x)[1]))
      legend(
        "right",
        legendnames,
        fill = paramcolors[1:(dim(x)[1])],
        border = "white",
        bty = "n"
      )
    }
  }

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

################################################################################################
################################################################################################
# Returns long form dataframe

longFormThis <- function(outs, SA, SAConditions) {
  # target_statistic (mean vulnerability etc)
  # community (Iztapalapa, All of Mexico City)
  # param (budget)
  # outcome_name (first order sensitivity index, total order sensitivity index, min, max, mean)
  # outcome_value (some floating point) value

  longSA <- reshape2::melt(SA)
  longSA <- dplyr::select(longSA, -sample_size)

  apply_margin <- 3:length(dim(Y))

  summaryOuts <- apply(outs, apply_margin, appl_summary_statistics)

  if (SAConditions$municip) {
    dimnames(summaryOuts) <- list("outcome_name" = SAConditions$outStats,
                                  "community" = outs$community,
                                  "target_statistic" = outs$target_statistic)
  } else {
    dimnames(summaryOuts) <- list("outcome_name" = SAConditions$outStats,
                                  "target_statistic" = oMetricNames)
  }

  longOuts <- reshape2::melt(summaryOuts)

  longOuts$input_parameter <- "All"

  long <- dplyr::full_join(longSA, longOuts, by = c("input_parameter", "outcome_name", "target_statistic", "value"))


  return(long)
}

appl_summary_statistics <- function(matr) {
  meann <- mean(matr)
  maxx <- max(matr)
  minn <- min(matr)

  return(c(meann, maxx, minn))
}
