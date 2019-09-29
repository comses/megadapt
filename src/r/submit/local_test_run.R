library(megadaptr)
setwd("../submit/")

# the following function is needed to get the params list that the rsp template produces
write_params_list <- function(line) {
  return_string <- "list("
  for (p in 1:(length(line)-1)) {
    new_string <- paste(names(line[p]), " = ", line[p], "," , sep = "")
    return_string <- paste(return_string, new_string)
  }
  new_string <- paste(names(line[p+1]),"=", line[p+1])
  return_string <- paste(return_string, new_string, ")")
}


#setup
ABMats <- megadaptr::cli_root(c("--db-config","../megadaptr/inst/experiment/db-sqlite.json",
                                "vbsa",
                                "setup",
                                "--experiment-config", "config.json",
                                "--experiment-number", "localtest001",
                                "--maximum-exponent", 2,
                                "--simulate-function", "one_megadapt_superficial_params_simulator"))


#run
jobN <- 1
for (mslice in 1:dim(ABMats)[3]) {
  for (mrow in 1:dim(ABMats)[1]) {
    # mats_line <- ABMats[mrow,,mslice]
    params <- write_params_list(ABMats[mrow,,mslice])
    megadaptr::cli_root(c("--db-config", "../megadaptr/inst/experiment/db-sqlite.json",
                          "vbsa",
                          "run",
                          "--experiment-config","config.json",
                          "--id", jobN,
                          "--params", params,
                          "--sample_n", mrow,
                          "--ABMat", mslice,
                          "--simulate-function", "one_megadapt_superficial_params_simulator"))
    jobN <- jobN + 1
  }
}

#reduce
megadaptr::cli_root(c("--db-config","../megadaptr/inst/experiment/db-sqlite.json",
                      "vbsa",
                      "reduce",
                      "--experiment-config","config.json"))
