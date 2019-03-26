
# (Originally in file VBSAMatrices.R)
# This file creates the matrices used in the Variance-Based Sensitivity Analysis of MEGADAPT
# For this version, we are going to test the effect of 4 parameters, whose particular values are taken from a linear transformation of the Sobol' sequence.

#' Docs
#'

createLinearMatrices <- function(N,k=4) {

  # effectivity_newInfra, initially set at 0.07
  par1_min=0.01
  par1_max=0.3

  #effectivity_mantenimiento, initially set at 0.07
  par2_min=0.1
  par2_max=0.3

  #decay_infra, initially set at 0.01
  par3_min=0.001
  par3_max=0.1

  #Budget, initially set at 1800
  par4_min=100
  par4_max=3000

  interc=rep(0,k)
  interc[1]=par1_min
  interc[2]=par2_min
  interc[3]=par3_min
  interc[4]=par4_min
  slope=rep(0,k)
  slope[1]=par1_max - par1_min
  slope[2]=par2_max - par2_min
  slope[3]=par3_max - par3_min
  slope[4]=par4_max - par4_min

  S=qrng::sobol(N+4,2*k,skip=1)

  for (i in 1:(2*k)) {
    if (i<=k) {
      S[,i]=S[,i]*slope[i]+interc[i]
    } else {
      S[,i]=S[,i]*slope[i-k]+interc[i-k]
    }
  }

  ABMats=array(dim=c(N,k,(k+2)))
  ABMats[,,]=S[1:N,1:k]
  ABMats[,,2]=S[5:(N+4),(k+1):(2*k)] #Pairing A with B taken from 4 rows below (p.38 from book chapter)
  for (j in 1:k) {
    ABMats[,j,(j+2)]=S[5:(N+4),j+k]
  }

  return(ABMats)

  # ABpath<-paste("/VBSAMatrices/ABMats",as.character(N),".rds",sep="")

  # saveRDS(ABMats,ABpath)

}

################################################################################################
################################################################################################

#(Originally in file VBSAmain.R)
#Running the model and retrieving the variables of interest

run<-function(megadapt) {

  # set.seed(1000)
  results <- simulate_megadapt(megadapt)

  lastT<-max(results$time_sim)
  Vlast<-subset(results,time_sim==lastT,select=c(vulnerability_Ab,vulnerability_D))
  return(Vlast)
}


################################################################################################
################################################################################################

#(Originally in file VBSA.R)
#Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.

VBSA<-function(exp.min,exp.max,simyears=5,k=4,realmodel) {

  maxN<-2^exp.max

  #Create matrix
  ABMats<-createLinearMatrices(maxN,k)

  # #Names of matrices change according to N
  # ABname<-paste("ABMats",as.character(maxN),".rds",sep="")
  # #Assign files to A and B matrices and ABi array
  # ABMats<-readRDS(paste("VBSAMatrices/",ABname,sep=""))


  #Create array of results: number of rows corresponds to number of model metrics, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
  #It's kind of awkward, but it was the only way I could find to organize the data, given that modelMetrics can have more than one output
  Y<-array(dim=c(2,maxN,(dim(ABMats)[3])))

  #Run simulations
  if (realmodel) {
    megadapt<-initialize_megadapt(simyears)
    Y<-apply(ABMats,c(1,3),function(x) modelMetrics(x,megadapt))
  } else {
    Y<-apply(ABMats,c(1,3),function(x) toyFunction(x))
  }


  #Calculate sensitivity indices
  #rows<->parameters, columns<->N, third<->(output metrics)*2: first Si for all metrics, then STi for all metrics
  resultss<-array(dim=c(k,(exp.max-exp.min+1),4))

  for (i in exp.min:exp.max) {
    N<-2^i

    Sis<-apply(Y,1,function(x) calc.Si(x,N,k))
    Sis
    STis<-apply(Y,1,function(x) calc.STi(x,N,k))

    resultss[1:k,(i-exp.min+1),]<-c(Sis,STis)
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
                       'budget')) {
    if (!is.na(x[param_ind])) {
      custom_params[[param_name]] <- x[param_ind]
    }
    param_ind <- param_ind + 1
  }

  #
  # Param Setup
  #
  params <- do.call(create_params, custom_params)

  # Add parameters to the megadapt object
  megadapt$params<-params

  # print(megadapt$params)

  #Run model and get results from last year
  Vlast<-run(megadapt)
  means<-apply(Vlast,2,function(x) mean(x,na.rm=T))

  return(means)
}



################################################################################################
################################################################################################
#(Originally in file VBSA.R)

# Functions to calculate sensitivity indices

calc.Si<-function(y,N,k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat<<-calc.Vhat(y,N)
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
  vti<-(sum( (Ares - y) ^2 ))/(2*N)
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

  # Study Area Setup
  #
  study_area <-
    rgdal::readOGR(
      data_dir("Layer_MEGADAPT_Oct2018.shp"),
      stringsAsFactors = FALSE,
      integer64 = "warn.loss"
    ) # for flooding model
  # Simulation runs only for the city (CDMX) estado=="09"
  study_area <- subset(study_area, estado == "09")
  study_area@data <- create_study_data(study_area@data)

  #
  # Ponding Model Setup
  #
  ponding_models <- load_ponding_models(data_dir(""))

  #
  # Water Scarcity Model Setup
  #
  water_scarcity_model <- create_water_scarcity_model(study_area@data)

  #
  # Climate Scenario Setup
  #
  climate_scenario <- read.csv(data_dir("df_prec_esc_85_85.csv"))

  #
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

  #
  # Mental Model Setup
  #
  mm_water_operator_s_lim <- data.frame(read.csv(data_dir("DF101215_GOV_AP modificado PNAS.limit.csv"),
                                                 skip = 1, header = T))[, -c(1, 2, 21)]
  mm_water_operator_d_lim <- data.frame(read.csv(data_dir("SACMEX_Drenaje_limit_SESMO.csv"),
                                                 skip = 1, header = T))[, -c(1, 2)]
  mm_iz <- data.frame(read.csv(data_dir("I080316_OTR.limit.csv"), skip = 1, header = T))[, -c(1, 2)]

  mental_models <- create_mental_models(
    mm_water_operator_d_lim = mm_water_operator_d_lim,
    mm_water_operator_s_lim = mm_water_operator_s_lim,
    mm_iz = mm_iz
  )

  #
  # Build Main Model
  #
  megadapt <<- create_megadapt(
    climate_scenario = climate_scenario,
    mental_models = mental_models,
    params = 0L,
    ponding_models = ponding_models,
    study_area = study_area,
    water_scarcity_model = water_scarcity_model,
    value_function_config = value_function_config
  )
  megadapt
}

#Toy function to check everything else about SA makes sense
toyFunction <- function(x) {
  Vlast<-matrix(c(x,x/2),ncol=2)
  means<-apply(Vlast,2,function(x) mean(x,na.rm=T))
}




