#!/usr/bin/env Rscript

megadaptr::cli_root(c("--db-config","inst/experiment/db-postgres.json",
                      "vbsa",
                      "reduce",
                      "--experiment-config","config.json"))


#
# SA_config <- jsonlite::read_json("config.json")
# SA_conds <- SA_config[[1]]
# SA_params <- SA_config[[2]]
# megadapt_conds <- SA_config[[3]]
#
# Y <- readRDS("Ya")
#
# testing <- as.numeric(c(NA, NA, NA, NA))
#
# any_NA_rows <- apply(Y,c(1,3,4), function(x) identical(x,testing))
#
# if (any(any_NA_rows)) {print("Missed experiment(s)")}
#
# SA_indices <- calcSensitivityIndices(SA_conditions = SA_conds,
#                                      SA_params = SA_params,
#                                      Y = Y)
#
# long_out <- longFormThis(outs = Y,
#                        SA = SA_indices,
#                        SA_conditions = SA_conds)
#
# saveRDS(long_out, "SAResults")
