# This file summarize outputs by delegations
flood_output <- tapply(TS_res$encharca, list(TS_res$municipio, TS_res$year_sim), mean, na.rm = T)
scarcity_output <- tapply(TS_res$days_wn_water_year, list(TS_res$municipio, TS_res$year_sim), mean, na.rm = T)
adaptation_AB_output <- tapply(TS_res$sensitivity_Ab, list(TS_res$municipio, TS_res$year_sim), mean, na.rm = T)
adaptation_D_output <- tapply(TS_res$sensitivity_D, list(TS_res$municipio, TS_res$year_sim), mean, na.rm = T)
SP_output <- tapply(TS_res$social_pressure, list(TS_res$municipio, TS_res$year_sim), mean, na.rm = T)
