#!/usr/bin/env Rscript

print("Inicio")

SA_config <- jsonlite::read_json("config.json")
SA_conds <- SA_config[[1]]
SA_params <- SA_config[[2]]
megadapt_conds <- SA_config[[3]]

Y <- readRDS("Ya")

SA_indices <- calcSensitivityIndices(SA_conditions = SA_conds,
                                     SA_params = SA_params,
                                     Y = Y)

long_out <- longFormThis(outs = Y,
                       SA = SA_indices,
                       SA_conditions = SA_conds)

saveRDS(long_out, "SAResults")

print("Fin")
