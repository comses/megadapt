#!/usr/bin/env Rscript

argms <- commandArgs(trailingOnly = TRUE)
print(argms)

params <- eval(parse(text = argms[1]))
print(params)

arow <- argms[2]
aslice <- argms[3]
jobN <- argms[4]


megadaptr::cli_root(c("--db-config", "inst/experiment/db-postgres.json",
                       "vbsa",
                       "run",
                       "--experiment-config","config.json",
                       "--id", jobN,
                       "--params", params,
                       "--sample_n", arow,
                       "--ABMat", aslice))

# SA_config <- jsonlite::read_json("config.json")
# SA_conditions <- SA_config[[1]]
# SA_params <- SA_config[[2]]
# ABMats <- readRDS("ABMats")
# model_f <- megadaptr:::one_megadapt_superficial_params_simulator(params)

# Y <- readRDS("Ya")
# Y[arow,aslice,,]<- model_f
# saveRDS(Y,"Ya")

#paramvect<-unlist(params)
#mats<-readRDS("outmat")
#mats<-rbind(mats,paramvect)
#saveRDS(mats,"outmat")
