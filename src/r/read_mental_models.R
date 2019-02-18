# read Mental model matrix
MM_WaterOperator_S <- data.frame(read.csv(paste(path_td, "DF101215_GOV_AP modificado PNAS.weighted.csv", sep = ""), skip = 1, header = T))[, -c(1, 2, 21)]
MM_WaterOperator_D <- data.frame(read.csv(paste(path_td, "SACMEX_Drenaje__weighted_SESMO.csv", sep = ""), skip = 1, header = T))[, -c(1, 2)]
MM_WaterOperator_S_lim <- data.frame(read.csv(paste(path_td, "DF101215_GOV_AP modificado PNAS.limit.csv", sep = ""), skip = 1, header = T))[, -c(1, 2, 21)]
MM_WaterOperator_D_lim <- data.frame(read.csv(paste(path_td, "SACMEX_Drenaje_limit_SESMO.csv", sep = ""), skip = 1, header = T))[, -c(1, 2)]

# name criteria
Names_criteria_sacmex_S <- colnames(MM_WaterOperator_S_lim)[-c(1:5)]
Names_criteria_sacmex_D <- colnames(MM_WaterOperator_D_lim)[-c(1, 2)]

# criteria values
Criteria_sacmcx_Ab <- MM_WaterOperator_S_lim$Antiguedad[-c(1:5)]
Criteria_sacmcx_D <- MM_WaterOperator_D_lim$Antiguedad[-c(1:2)]

# alternative names
Names_Alternative_sacmex_S <- colnames(MM_WaterOperator_S_lim)[c(1:5)]
Names_Alternative_sacmex_D <- colnames(MM_WaterOperator_D_lim)[c(1, 2)]

alternative_weights_S <- MM_WaterOperator_S_lim$Antiguedad[c(1:5)]
alternative_weights_D <- MM_WaterOperator_D_lim$Antiguedad[c(1:2)]

# Resident Iztapalapa
MM_Iz <- data.frame(read.csv(paste(path_td, "I080316_OTR.limit.csv", sep = ""), skip = 1, header = T))[, -c(1, 2)]
Names_criteria_Resident_Iz <- colnames(MM_Iz)[-c(1:5)]
Names_Alternative_Resident_Iz <- colnames(MM_Iz)[c(1:5)]

Criteria_residents_Iz <- MM_Iz$Compra.de.agua[-c(1:5)]
alternative_weights_Iz <- MM_Iz$Compra.de.agua[c(1:5)]
