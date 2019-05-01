
# (Originally in file VBSAMatrices.R)
# This file creates the matrices used in the Variance-Based Sensitivity Analysis of MEGADAPT
# For this version, we are going to test the effect of 4 parameters, whose particular values are taken from a linear transformation of the Sobol' sequence.

#' Docs
#'

createLinearMatrices <- function(SAConds,SAParams) {
  N <- 2 ^ SAConds$exp.max
  k <- length(SAParams)

  interc=rep(0,k)
  for (i in 1:k) {
    interc[i]<-SAParams[[i]]$min
  }

  slope=rep(0,k)
  for (j in 1:k) {
    slope[j]<-(SAParams[[j]]$max - SAParams[[j]]$min)
  }


  S=qrng::sobol(N+4,2*k,skip=1)

  for (i in 1:(2*k)) {
    if (i<=k) {
      S[,i]=S[,i]*slope[i]+interc[i]
      if (SAParams[[i]]$isInteger==T) {
        S[,i]=round(S[,i])
      }
    } else {
      S[,i]=S[,i]*slope[i-k]+interc[i-k]
      if (SAParams[[i-k]]$isInteger==T) {
        S[,i]=round(S[,i])
      }
    }
  }


  ABMats <- array(dim=c(N,k,(k+2)))
  ABMats[,,] <- S[1:N,1:k]
  ABMats[,,2] <- S[1:(N),(k+1):(2*k)]
  for (j in 1:k) {
    ABMats[,j,(j+2)] <- S[1:(N),j+k]
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

runn<-function(megadapt,oMetricNames) {

  # set.seed(1000)
  results <- simulate_megadapt(megadapt)

  lastT<-max(results$time_sim)
  Vlast<-subset(results,time_sim==lastT,select=oMetricNames)
  return(Vlast)
}


################################################################################################
################################################################################################

#(Originally in file VBSA.R)
#Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.

VBSA<-function(SAConditions,SAParams,oMetricNames) {
  if (SAConditions$onCluster) {
    plan(multisession)
  } else {
    plan(sequential)
  }

  exp.min <- SAConditions$exp.min
  exp.max <- SAConditions$exp.max
  maxN <- 2^exp.max
  k <- length(SAParams)
  #
  # #Create matrix
  # ABMats<-createLinearMatrices(maxN,k)

  # #Names of matrices change according to N
  # ABname<-paste("ABMats",as.character(maxN),".rds",sep="")
  # #Assign files to A and B matrices and ABi array
  # ABMats<-readRDS(paste("VBSAMatrices/",ABname,sep=""))


  #Create array of results: number of rows corresponds to number of model metrics, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
  #It's kind of awkward, but it was the only way I could find to organize the data, given that modelMetrics can have more than one output
  Y<-array(dim = c(length(oMetricNames),maxN,(dim(ABMats)[3])))
           # dimnames = list("OutMetric","N","ABi"))

  #Run simulations
  if (SAConditions$whichmodel=="custom") {
    megadapt<-initialize_megadapt(SAConditions$simyears)
    Y<-future.apply::future_apply(ABMats,c(1,3),function(x) modelMetrics(x,megadapt))

    #how to couple it to other models?

  } else if (SAConditions$whichmodel=="toy") {
    Y<-future.apply::future_apply(ABMats,c(1,3),function(x) toyFunction(x))
  } else if (SAConditions$whichmodel=="book") {
    Y<-future.apply::future_apply(ABMats,c(1,3),function(x) bookEx(x))
  }

  if (length(oMetricNames) > 1) {
    Y<-aperm(Y,c(2,3,1))
  }


  #Calculate sensitivity indices
  #rows<->parameters, columns<->N, third<->(output metrics)*2: first Si for all metrics, then STi for all metrics
  resultss <- array(dim=c(k,(exp.max - exp.min+1),(length(oMetricNames)*2)))

  if (length(oMetricNames) == 1){
    for (i in exp.min:exp.max) {
      N<-2^i

      Sis<-calc.Si(Y,N,k)
      Sis
      STis<-calc.STi(Y,N,k)
      resultss[1:k,(i-exp.min+1),]<-c(Sis,STis)
    }
  } else {
    for (i in exp.min:exp.max) {
      N<-2^i

      Sis<-apply(Y,3,function(x) calc.Si(x,N,k))
      Sis
      STis<-apply(Y,3,function(x) calc.STi(x,N,k))

      resultss[1:k,(i-exp.min+1),]<-c(Sis,STis)
    }
  }

  return(resultss)
}

# Function that gets the summary statistic from the variables of interest
modelMetrics<-function(x,megadapt){

  #Assign values to variables
  custom_params <- list()
  param_ind <- 1
  for (param_name in c('new_infrastructure_effectiveness_rate',
                       'maintenance_effectiveness_rate',
                       'infrastructure_decay_rate',
                       'budget',
                       'climate_scenario')) {
    if (!is.na(x[param_ind])) {
      custom_params[[param_name]] <- x[param_ind]
    }
    param_ind <- param_ind + 1
  }
  custom_params$n_steps <- SAConditions$simyears

  #
  # Param Setup
  #
  params <- do.call(create_params, custom_params)


  #
  Table_climate_scenarios=as.data.frame(read.csv(data_dir("climate_landuse_scenarios/db_escenarios_prec_esc_ids.csv"),header = T))

  #generate the path to the place where the data frame of the scenario is stored

  scenario_name=Table_climate_scenarios[which(Table_climate_scenarios$id==params$climate_scenario),]$path

  # Climate Scenario Setup
  #
  climate_scenario <- read.csv(data_dir(paste0("climate_landuse_scenarios/",scenario_name)))



  # Add parameters to the megadapt object
  megadapt$params <- params
  megadapt$climate_scenario <- climate_scenario

  # print(megadapt$params)

  #Run model and get results from last year
  Vlast<<-runn(megadapt)
  means<-apply(Vlast,2,function(x) mean(x,na.rm=T))

  return(means)
}



################################################################################################
################################################################################################
#(Originally in file VBSA.R)

# Functions to calculate sensitivity indices

calc.Si<-function(y,N,k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat<-calc.Vhat(y,N)
  Ares<-y[1:N,1]
  Bres<-y[1:N,2]
  vis<-apply(y[1:N,3:(k+2)],2,function(x) calc.Vi(x,N,Ares,Bres))
  print(vis)
  si<-vis/vhat
  si # returns a matrix where rows are the k parameters, columns are the metrics used as model output
}

calc.Vi<-function(y,N,Ares,Bres) {
  vi<-(sum( Bres * (y - Ares) )) / (N)
  vi
}


calc.STi<-function(y,N,k) {
  vhat<-calc.Vhat(y,N)
  Ares<-y[1:N,1]
  vti<-apply(y[1:N,3:(k+2)],2,function(x) calc.VTi(x,N,Ares))
  sti<-vti/vhat
  sti
}

calc.VTi<-function(y,N,Ares) {
  vti<-(sum((Ares - y) ^ 2 )) / (2*N)
  vti
}


calc.Vhat<-function(y,N) {
  fhat<-(sum( y[1:N,1]+y[1:N,2] )) / (2*N)
  vhat<-(sum( (y[1:N,1]-fhat)^2 + (y[1:N,2]-fhat)^2 )) / (2*N-1)
  vhat
}

################################################################################################
################################################################################################

#New functions

#Create the megadapt object, just without the parameters (copied from example.R)

initialize_megadapt <- function(n_years=5) {
  path_to_source <- "." # change path to use it
  path_td <- "../data/"
  path_to_output <- "../outputs/" # change path to use it

  # #Assign values to variables
  # custom_params <- list()
  # param_ind <- 1
  # for (param_name in c('new_infrastructure_effectiveness_rate',
  #                      'maintenance_effectiveness_rate',
  #                      'infrastructure_decay_rate',
  #                      'budget',
  #                      'climate_scenario')) {
  #   if (!is.na(x[param_ind])) {
  #     custom_params[[param_name]] <- x[param_ind]
  #   }
  #   param_ind <- param_ind + 1
  # }

  # #
  # # Param Setup
  # #
  # params <- do.call(create_params, custom_params)

  #
  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      data_dir("censusblocks/megadapt_wgs84.shp"),#input_layer.shp
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    ) # for flooding model
  # Simulation runs only for the city (CDMX) estado=="09"
  #study_area <- subset(study_area, estado == "09")
  study_area@data <- create_study_data(study_area@data)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(data_dir(""))

  # Flooding Model Setup
  #
  flooding_models <- load_flooding_models(data_dir(""))



  # Value Function Setup
  #
  fv_antiguedad_drenaje <- load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_drenaje.csv"))
  fv_antiguedad_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_antiguedad_escasez.csv"))
  fv_calidad_agua_sodio_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_calidad_agua_sodio_escasez.csv"))
  fv_falla_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_falla_escasez.csv"))
  fv_horas_servicio_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_horas_servicio_escasez.csv"))
  fv_presion_hidraulica_escasez <- load_value_function_config(data_dir("funciones_valor/csvs/fv_presion_hidraulica_escasez.csv"))
  fv_subsidencia <- load_value_function_config(data_dir("funciones_valor/csvs/fv_subsidencia.csv"))

  value_function_config <- create_value_function_config(
    sewer_age=fv_antiguedad_drenaje,
    shortage_age=fv_antiguedad_escasez,
    salt_water_quality=fv_calidad_agua_sodio_escasez,
    shortage_failures=fv_falla_escasez,
    hours_of_service_failure=fv_horas_servicio_escasez,
    hydraulic_pressure_failure=fv_presion_hidraulica_escasez,
    subsidence=fv_subsidencia
  )

  # Mental Model Setup
  #
  mm_water_operator_s_lim <- data.frame(read.csv(data_dir("/mental_models/DF101215_GOV_AP modificado PNAS.limit.csv"),
                                                 skip = 1, header = T))[, -c(1, 2, 21)]
  mm_water_operator_d_lim <- data.frame(read.csv(data_dir("/mental_models/SACMEX_Drenaje_limit_SESMO.csv"),
                                                 skip = 1, header = T))[, -c(1, 2)]
  mm_iz <- data.frame(read.csv(data_dir("/mental_models/I080316_OTR.limit.csv"), skip = 1, header = T))[, -c(1, 2)]

  mental_models <- create_mental_models(
    mm_water_operator_d_lim = mm_water_operator_d_lim,
    mm_water_operator_s_lim = mm_water_operator_s_lim,
    mm_iz = mm_iz
  )

  #
  # Build Main Model
  #
  megadapt <- create_megadapt(
    climate_scenario = 0L,
    mental_models = mental_models,
    params = 0L,
    ponding_models = ponding_models,
    flooding_models = flooding_models,
    study_area = study_area,
    value_function_config = value_function_config
  )
  megadapt
}

# Toy function to check everything else about SA makes sense
toyFunction <- function(x) {
  Vlast<-matrix(c(x,x/2),ncol=2)
  means<-apply(Vlast,2,function(x) mean(x,na.rm=T))
}

# Example from book chapter to confirm we are getting same values.
bookEx <- function(x) {
  y = 3 * (x[1] ^ 2) + 2 * (x[1] * x[2]) - 2 * x[3]
}

################################################################################################
################################################################################################
# Plots

plotConvergence <- function(x) {  #Plots both Si and STi vs N (Maximum of 5 metrics)
  Ns<-seq(SAConditions$exp.min,SAConditions$exp.max)
  numberPlots <- dim(x)[3]
  par(mfrow=c((numberPlots/2),2))
  paramcolors<-c("darkcyan","blue4","darkorchid","darkorange2","brown2","chartreuse3","skyblue","orange","magenta")
  for (i in 1:numberPlots){
    thisRange <- range(x[ , ,i])
    plot(Ns,x[1,,i],
         ylim=c(thisRange[1],thisRange[2] + 0.05),
         col=paramcolors[1],
         xlab="N",
         ylab="")
    if (i<=(numberPlots/2)) {
      title(main=bquote(.(oMetricNames[i]) ~ S[i]),
            ylab=bquote(S[i]))
    } else {
      title(main=bquote(.(oMetricNames[i-(numberPlots/2)]) ~ S[Ti]),
            ylab=bquote(S[Ti]))
    }
    for (j in 2:dim(x)[1]){
      points(Ns,x[j,,i],col=paramcolors[j])
    }
    legendnames<-paste("x", seq(1:dim(x)[1]))
    legend("right", legendnames,
           fill = paramcolors[1:(dim(x)[1])],
           border="white",
           bty="n")
  }
}

plotPieCharts <- function(x,expon,metric) {
  n <- expon - SAConditions$exp.min + 1
  if (n < 1) {stop("Exponent has to be equal or greater than exp.min")}
  metrOfInterest <- match(metric,oMetricNames)
  jump <- (dim(x)[3])/2
  sivals <- abs(x[ ,n, metrOfInterest])
  siperc <- round((sivals/sum(sivals))*100)
  silabl <- paste(siperc, " %")
  #add names of parameters
  stivals <- abs(x[ ,n, (metrOfInterest + jump)])
  stiperc <- round((stivals/sum(stivals))*100)
  stilabl <- paste(stiperc, " %")
  #same here
  par(mfrow=c(1,2))
  paramcolors<-c("darkcyan","blue4","darkorchid","darkorange2","brown2","chartreuse3","skyblue","orange","magenta")
  # if (length(sivals) <= length(paramcolors)) {
  #   piecolors <- paramcolors[1:length(sivals)]
  # } else {
  #   piecolors <- rainbow(length(sivals))
  # }
  pie(sivals, main = bquote(S[i]), labels = silabl, col = topo.colors(length(sivals)))
  pie(stivals, main = bquote(S[Ti]), labels = stilabl, col = topo.colors(length(sivals)))
  #To do: add percentages and labels
}

################################################################################################
################################################################################################
# # Setup functions
#
# setSAConds <- function(exp.min,
#                        exp.max,
#                        whichmodel,
#                        simyears = 40) {
#   list(
#     exp.min = exp.min,
#     exp.max = exp.max,
#     whichmodel = whichmodel,
#     simyears = simyears
#   )
# }
#
# setSAParams <- function(inputParamsNames,
#                         ){
#
# }


