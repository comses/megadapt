#!/usr/bin/env Rscript

argms <- commandArgs()

SA_config <- jsonlite::read_json("config.json")

SA_conditions <- SA_config[[1]]
SA_params <- SA_config[[2]]
megadapt_conds <- SA_config[[3]]

abs <- megadaptr:::createLinearMatrices(SA_conditions,SA_params)

# Y_rows <- dim(abs)[1] * dim(abs)[3]

# Y_long <- matrix(nrow=Y_rows,ncol=length(SA_params))

Y_third <- 17
Y_fourth <- length(megadapt_conds$out_metric_names) * length(megadapt_conds$out_stats)

Y_array <- array(numeric(),dim=c(dim(abs)[1],dim(abs)[3], Y_third, Y_fourth)

saveRDS(abs,"ABMats")
saveRDS(Y_array,"Ya")

# abs <- array( c(1:12), dim = c(4,3), dimnames = list("x_i"=c("p1","p2","p3","p4"),"sample_n"=c(1,2,3)))

# abs <- aperm(abs, c(2,1))

R.rsp::rfile("runcli.dag.rsp", args = list(ABMats=abs))
