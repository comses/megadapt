library(megadaptr)
source('../scenarios/util.R')

################################################################################################
################################################################################################

#(Originally in file VBSAConvergence.R)
#Convergence of Si and STi indices

# SA Conditions
SAConditions <- list(
  simyears=2,
  exp.min=1,
  exp.max=2,
  whichmodel="modelMetrics", # other options are: "toyFunction" and "bookEx"
  onCluster=F,
  municip=T,
  outStats=c("mean","max","min"),
  oMetricNames = c("household_potable_water_vulnerability","household_sewer_vulnerability","flooding_index","ponding_index","scarcity_index"),
  # oMetricNames=c("potable_water_vulnerability_index","non_potable_water_vulnerability_index"),
  # oMetricNames = c("non_potable_percent_lacking","potable_water_system_intervention_count","potable_water_infrastructure_age","potable_water_vulnerability_index","non_potable_water_vulnerability_index"),
  communities = c(
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
)


runMod <- F

# Input Parameters and their Names
noParams <- 4 #number of parameters to take into account

SAParams <- list(
  p1 = list(
    name = "new_infrastructure_effectiveness_rate",
    min = 0.01,
    max = 0.3,
    isInteger = F
  ),
  p2 = list(
    name = "maintenance_effectiveness_rate",
    min = 0.1,
    max = 0.3,
    isInteger = F
  ),
  p3 = list(
    name = "infrastructure_decay_rate",
    min = 0.001,
    max = 1,
    isInteger = F
  ),
  p4 = list(
    name = "budget",
    min = 24,
    max = 2428,
    isInteger = T
  ),
  p5 = list(
    name = "climate_scenario",
    min = 1,
    max = 12,
    isInteger = T
  )
)


SAParams <- SAParams[1:noParams]





if (SAConditions$whichmodel == "book") {
  noParams <- 3 #number of parameters to take into account
  SAParams <- list(
    p1 = list(
      name = "effectivity_newInfra",
      min = 0,
      max = 1,
      isInteger = F
    ),
    p2 = list(
      name = "effectivity_mantenimiento",
      min = 0,
      max = 1,
      isInteger = F
    ),
    p3 = list(
      name = "decay_infra",
      min = 0,
      max = 1,
      isInteger = F
    )
  )
  SAParams <- SAParams[1:noParams]
  # Output metrics
  SAConditions$oMetricNames <- c("vulnerability_Ab")

  SAConditions$municip = FALSE
}

################################################################################################
################################################################################################


resultsss <- megadaptr:::VBSA(SAConditions, SAParams)

# saveRDS(resultsold, "results")

print(SAConditions)

