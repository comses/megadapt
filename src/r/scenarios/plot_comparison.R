#!/usr/bin/env Rscript

library("argparse")
library("RPostgreSQL")
library("DBI")
library("dplyr")
library("dbplyr")
library(ggplot2)
library(tidyr)
library("fs")

#parser <- ArgumentParser(description='Run model')


#parser$add_argument("--megaframe",
#		    type="character", required=T, help="path to megaframe")

#parser$add_argument("--plot_path",
 #                   type="character", required=T, help="path to plot")

#args <- parser$parse_args()


drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=9000,
                  user="fidel",
                  host="localhost")

params <- tbl(conn, 'params')
results <- tbl(conn, 'results')

meta <- tibble::tribble(
  ~colname, ~name,
  "water_vulnerability_index", "Mean Water Vulnerability Index",
  "water_system_intervention_count", "Mean Water System Interventions",
  "water_sensitivity_index", "Mean Water Sensitivity Index",
  "water_infrastructure_age", "Mean Water Infrastructure Age",
  "percent_lacking", "Percent Lacking Public Infrastructure"
)


query <- results %>%
  inner_join(params %>% filter(half_sensitivity_ab == 10,
                               half_sensitivity_d == 10,
                               infrastructure_decay == 0.1,
                               budget==1200,
                               effectiveness_maintenance == 0.1), c("param_id" = "param_id"))

x <- group_by(query,
    half_sensitivity_ab,
    half_sensitivity_d,
    budget,
    infrastructure_decay,
    effectiveness_new_infra,
    effectiveness_maintenance,
    steps,
    year_sim
  )
#
# dbWriteTable(conn = conn, name = "mean_results", value = x %>% collect() %>% ungroup(), row.names = FALSE, append = FALSE)
#   # %>%
y <- dplyr::summarize(x,
    potable_water_vulnerability_index=mean(potable_water_vulnerability_index, na.rm = TRUE),
    nonpotable_water_vulnerability_index=mean(non_potable_water_vulnerability_index, na.rm = TRUE),
    potable_water_system_intervention_count=mean(potable_water_system_intervention_count),
    nonpotable_water_system_intervention_count=mean(non_potable_water_system_intervention_count),
    potable_water_sensitivity_index=mean(potable_water_sensitivity_index),
    nonpotable_water_sensitivity_index=mean(non_potable_water_sensitivity_index),
    potable_water_infrastructure_age=mean(potable_water_infrastructure_age),
    nonpotable_water_infrastructure_age=mean(non_potable_water_infrastructure_age),
    potable_percent_lacking = mean(potable_percent_lacking),
    nonpotable_percent_lacking = mean(non_potable_percent_lacking))
 z <- y %>% collect()
#
potable_non_potable_comparison <- gather(z,
    key = "name",
    value = "value",
    potable_water_vulnerability_index,
    nonpotable_water_vulnerability_index,
    potable_water_system_intervention_count,
    nonpotable_water_system_intervention_count,
    potable_water_sensitivity_index,
    nonpotable_water_sensitivity_index,
    potable_water_infrastructure_age,
    nonpotable_water_infrastructure_age,
    potable_percent_lacking,
    nonpotable_percent_lacking
 ) %>%
   extract(name, into = c("system", "statistic"), "([[:alnum:]]+)_(.+)") %>%
   mutate(statistic = recode(statistic, !!! (meta %>% spread(colname, name) %>% as.list))) %>%
   mutate(system = recode(system, potable = "Potable", nonpotable = "Non Potable")) %>%
   rename(Budget=budget)
#
 potable_non_potable_comparison$effectiveness_new_infra <- as.factor(potable_non_potable_comparison$effectiveness_new_infra)
#
 the_plot <- ggplot(potable_non_potable_comparison, aes_string(x = "year_sim", y = "value", color = "effectiveness_new_infra")) +
   xlab("Year") +
   geom_line() +
   facet_wrap(vars(system, statistic), scales = "free_y", ncol = 5)
 the_plot
# ggsave(the_plot, file="plot_example.png")
#
# #select(non_potable_water_infrastructure_age, year_sim)
#
# # mean_results <- tibble::as_tibble(query)
#
# # dbWriteTable(conn = conn, name = "mean_results", value = mean_results, row.names = FALSE, append = FALSE)
