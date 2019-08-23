#!/usr/bin/env Rscript

argms <- commandArgs()
print(argms)

params <- eval(parse(text= argms[6]))
print(params)

arow <- as.numeric(argms[7])
aslice <- as.numeric(argms[8])

# SA_config <- jsonlite::read_json("config.json")
# SA_conditions <- SA_config[[1]]
# SA_params <- SA_config[[2]]

# ABMats <- readRDS("ABMats")

model_f <- megadaptr:::megadapt_superficial_params_simulator_alone(params)

Y <- readRDS("Ya")

Y[arow,aslice,,]<- model_f

saveRDS(Y,"Ya")

#paramvect<-unlist(params)
#mats<-readRDS("outmat")
#mats<-rbind(mats,paramvect)
#saveRDS(mats,"outmat")
