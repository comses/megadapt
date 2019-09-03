#!/usr/bin/env Rscript

argms <- commandArgs(trailingOnly = TRUE)

source("../submit/create_config.R")

create_config(argms[1])


ABMats <- megadaptr::cli_root(c("--db-config","inst/experiment/db-postgres.json",
                               "vbsa",
                               "setup",
                               "--experiment-config","config.json"))

# Y_rows <- dim(abs)[1] * dim(abs)[3]
# Y_long <- matrix(nrow=Y_rows,ncol=length(SA_params))

# abs <- array( c(1:12), dim = c(4,3), dimnames = list("x_i"=c("p1","p2","p3","p4"),"sample_n"=c(1,2,3)))
# abs <- aperm(abs, c(2,1))
