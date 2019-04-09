library(megadaptr)
source('../scenarios/util.R')


################################################################################################
################################################################################################

#(Originally in file VBSAConvergence.R)
#Convergence of Si and STi indices

# SA Conditions
SAConditions <- list(
  simyears=1,
  exp.min=1,
  exp.max=1,
  whichmodel="custom",
  onCluster=F
)


# Input Parameters and their Names
noParams <- 5 #number of parameters to take into account

SAParams <- list(
  p1 = list(
    name = "effectivity_newInfra",
    min = 0.01,
    max = 0.3,
    isInteger=F
  ),
  p2 = list(
    name = "effectivity_mantenimiento",
    min = 0.1,
    max = 0.3,
    isInteger=F
  ),
  p3 = list(
    name = "decay_infra",
    min = 0.001,
    max = 1,
    isInteger=F
  ),
  p4 = list(
    name = "Budget",
    min = 24,
    max = 2428,
    isInteger=F
  ),
  p5 = list(
    name = "climate_scenario",
    min = 1,
    max = 12,
    isInteger=T
  )
)


SAParams <- SAParams[1:noParams]


# Output metrics
oMetricNames <- c("potable_water_vulnerability_index","non_potable_water_vulnerability_index")


if (SAConditions$whichmodel=="book"){
  noParams <- 3 #number of parameters to take into account
  SAParams <- list(p1 = list(
      name = "effectivity_newInfra",
      min = 0,
      max = 1,
      isInteger=F
    ), p2 = list(
      name = "effectivity_mantenimiento",
      min = 0,
      max = 1,
      isInteger=F
    ),p3 = list(
      name = "decay_infra",
      min = 0,
      max = 1,
      isInteger=F
    ) )
  SAParams <- SAParams[1:noParams]
  # Output metrics
  oMetricNames <- c("vulnerability_Ab")
}




# #stt<-system.time(results<-VBSA(exp.min,exp.max,simyears,k,realmodel))
# #results
#
# results<-VBSA(exp.min,exp.max,simyears,k,realmodel)

################################################################################################
################################################################################################

ABMats <- createLinearMatrices(SAConditions,SAParams)

resultsold <- VBSA(SAConditions,SAParams,oMetricNames)


