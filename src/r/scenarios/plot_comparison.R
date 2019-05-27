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
                  port=2222,
                  user="fidel",
                  host="localhost")

params <- tbl(conn, 'params_test1')
results <- tbl(conn, 'results_test1')

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
                               effectiveness_new_infra==0.05,
                               effectiveness_maintenance == 0.1), c("param_id" = "key"))

x <- group_by(query,
    half_sensitivity_ab,
    half_sensitivity_d,
    budget,
    infrastructure_decay,
    effectiveness_new_infra,
    effectiveness_maintenance,
    steps,
    year
  )
#
# dbWriteTable(conn = conn, name = "mean_results", value = x %>% collect() %>% ungroup(), row.names = FALSE, append = FALSE)
#   # %>%
y <- dplyr::summarize(x,
    household_potable_water_vulnerability=mean(household_potable_water_vulnerability, na.rm = TRUE),
    household_sewer_vulnerability=mean(household_sewer_vulnerability, na.rm = TRUE),
    sacmex_potable_maintenance_intervention_count=mean(sacmex_potable_maintenance_intervention_count),
    sacmex_sewer_maintenance_intervention_count=mean(sacmex_sewer_maintenance_intervention_count),
    sacmex_potable_new_infrastructure_intervention_count=mean(sacmex_potable_new_infrastructure_intervention_count),
    sacmex_sewer_new_infrastructure_intervention_count=mean(sacmex_sewer_new_infrastructure_intervention_count),
    household_potable_water_sensitivity=mean(household_potable_water_sensitivity),
    household_sewer_sensitivity=mean(household_sewer_sensitivity),
    potable_water_infrastructure_age=mean(potable_water_infrastructure_age),
    sewer_infrastructure_age=mean(sewer_infrastructure_age),
    household_sewer_system_lacking_percent = mean(household_sewer_system_lacking_percent),
    household_potable_system_lacking_percent = mean(household_potable_system_lacking_percent))
 z <- y %>% collect()
#
potable_non_potable_comparison <- gather(z,
    key = "name",
    value = "value",
    household_potable_water_vulnerability,
    household_sewer_vulnerability,
    sacmex_potable_maintenance_intervention_count,
    sacmex_sewer_maintenance_intervention_count,
    sacmex_potable_new_infrastructure_intervention_count,
    sacmex_sewer_new_infrastructure_intervention_count,
    household_potable_water_sensitivity,
    household_sewer_sensitivity,
    potable_water_infrastructure_age,
    sewer_infrastructure_age,
    household_sewer_system_lacking_percent,
    household_potable_system_lacking_percent
 ) %>%
   extract(name, into = c("system", "statistic"), "([[:alnum:]]+)_(.+)") %>%
   mutate(statistic = recode(statistic, !!! (meta %>% spread(colname, name) %>% as.list))) %>%
   mutate(system = recode(system, potable = "Potable", nonpotable = "Non Potable")) %>%
   rename(Budget=budget)
#
 potable_non_potable_comparison$Budget <- as.factor(potable_non_potable_comparison$Budget)
#
 the_plot <- ggplot(potable_non_potable_comparison, aes_string(x = "year", y = "value", color = "Budget")) +
   xlab("Year") +
   geom_line() +
   facet_wrap(vars(system, statistic), scales = "free_y", ncol = 6)
 the_plot
# ggsave(the_plot, file="plot_example.png")
#
# #select(non_potable_water_infrastructure_age, year_sim)
#
# # mean_results <- tibble::as_tibble(query)
#
# # dbWriteTable(conn = conn, name = "mean_results", value = mean_results, row.names = FALSE, append = FALSE)
