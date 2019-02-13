#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

effectivity_newInfra=as.numeric(args[1])
effectivity_mantenimiento=as.numeric(args[2])
decay_infra=as.numeric(args[3])
time_simulation=as.numeric(args[4])
Budget=as.numeric(args[5])
#climate scenario
#path_to_source<-"c:/Users/abaezaca/Dropbox (ASU)/MEGADAPT/SHV/ABM_Rversion/MEGADAPT_APP/"
path_to_source<-"."
setwd(path_to_source)
#run setup.R and cycle.R 
    source("r/setup.R")
    source("r/cycle.R")
