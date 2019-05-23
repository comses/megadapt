#!/usr/bin/env Rscript

library("argparse")
library("dplyr")
library("DBI")
library("RPostgreSQL")

parser <- ArgumentParser(description='Run model')


parser$add_argument("--path",
		    type="character", required=T, help="?")

args <- parser$parse_args()



column_names <- c("effectiveness_new_infra",
"effectiveness_maintenance",
"steps",
"infrastructure_decay",
"budget",
"half_sensitivity_d",
"half_sensitivity_ab",
"rep")
#path <- "/Users/fidel/patung/megadapt/output/trial/sim_0.1_0.1_40_0.1_120_5_5_0.rds"


get_parameter_values <- function(path, key){
  the_name <- basename(path)
  the_params_text <- substr(the_name, 5, nchar(the_name)-4)
  df_params <- tibble::as_tibble(data.frame(t(as.numeric(unlist(strsplit(the_params_text, "_"))))))
  names(df_params) <- column_names
  df_params <- df_params %>% mutate(param_id = key)
  #df <- cbind(df, readRDS(path))
  #df <- df %>% mutate(path = path)
  results <- readRDS(path) %>% mutate(param_id = key)
  list(df_params = df_params, results = results)
}
#df %>% mutate(z = purrr::map(df$x, function(x) tibble::tibble(z=rep(1, x))))


path = args$path
setwd(path)
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt")
#conn <- dbConnect(RSQLite::SQLite(), "mega_frame")
file.names <- dir(path, pattern =".rds")
# tibbble <- tibble::tibble(effectiveness_new_infra = numeric(),
#                effectiveness_maintenance = numeric(),
#                steps = numeric(),
#                infrastructure_decay = numeric(),
#                budget = numeric(),
#                half_sensitivity_d = numeric(),
#                half_sensitivity_ab = numeric(),
#                rep = numeric(),
#                path = character())
for(i in 1:length(file.names)){
  print(i)
  single_simulation_results <- get_parameter_values(file.names[i], i)
  dbWriteTable(conn = conn, name = "params", value = single_simulation_results$df_params, row.names = FALSE, append = TRUE)
  dbWriteTable(conn = conn, name = "results", value = single_simulation_results$results, row.names = FALSE, append = TRUE)
}


#tibbble <- tibbble %>% mutate(results = purrr::map(tibbble$path, function(x) results=tibble::tibble(readRDS(x))))

#gg <- get_parameter_values("/Users/fidel/patung/megadapt/output/trial/sim_0.1_0.1_40_0.1_120_5_5_0.rds", 1)
