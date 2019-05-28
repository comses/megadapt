library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)

sdf <- sf::st_read('inst/rawdata/censusblocks/megadapt_wgs84_v5.gpkg') %>%
  dplyr::rename(geometry=geom) %>% select(censusblock_id,geometry)

filter_results <- function(params, results) {
  results %>%
    inner_join(params %>% filter(half_sensitivity_ab == 10,
                                 half_sensitivity_d == 10,
                                 infrastructure_decay == 0.1,
                                 effectiveness_new_infra==0.05,
                                 effectiveness_maintenance == 0.1), c("param_id" = "key"))
}

get_db_budget <- function(params, results) {
  filter_results(params, results) %>%
    group_by(
      censusblock_id,
      half_sensitivity_ab,
      half_sensitivity_d,
      budget,
      infrastructure_decay,
      effectiveness_new_infra,
      effectiveness_maintenance,
      steps
    ) %>%
    summarise(
      sacmex_potable_maintenance_intervention_presence=sum(as.integer(sacmex_potable_maintenance_intervention_presence)),
      sacmex_sewer_maintenance_intervention_presence=sum(as.integer(sacmex_sewer_maintenance_intervention_presence)),
      sacmex_potable_new_infrastructure_intervention_presence=sum(as.integer(sacmex_potable_new_infrastructure_intervention_presence)),
      sacmex_sewer_new_infrastructure_intervention_presence=sum(as.integer(sacmex_sewer_new_infrastructure_intervention_presence))
    ) %>%
    collect() %>%
    rename(sewer_maintenance=sacmex_sewer_maintenance_intervention_presence,
           sewer_new_infra=sacmex_sewer_new_infrastructure_intervention_presence,
           potable_maintenance=sacmex_potable_maintenance_intervention_presence,
           potable_new_infra=sacmex_potable_new_infrastructure_intervention_presence)
}

# Get data from split run

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=2222,
                  user="fidel",
                  host="localhost")

split_params <- tbl(conn, 'params_split_1')
split_results <- tbl(conn, 'results_split_1')

split_df <- get_db_budget(split_params, split_results)

# Get data from non split run

non_split_params <- tbl(conn, 'params_non_split_1')
non_split_results <- tbl(conn, 'results_non_split_1')

non_split_df <- get_db_budget(non_split_params, non_split_results)

# Union the two datasets together

#df <- union_all(split_df %>% mutate(budget_strategy = 'split'), non_split_df %>% mutate(budget_strategy = 'non_split'))

plot_interventions <- function(polygons, df, col_name) {
  df <- polygons %>% inner_join(df)
  ggplot(df, aes_string(fill = col_name)) +
    geom_sf(size=0.05)
}



p1 <- plot_interventions(sdf, split_df, 'sewer_maintenance')
p2 <- plot_interventions(sdf, split_df, 'sewer_new_infra')
p3 <- plot_interventions(sdf, split_df, 'potable_maintenance')
p4 <- plot_interventions(sdf, split_df, 'potable_new_infra')
p5 <- plot_interventions(sdf, non_split_df, 'sewer_maintenance')
p6 <- plot_interventions(sdf, non_split_df, 'sewer_new_infra')
p7 <- plot_interventions(sdf, non_split_df, 'potable_maintenance')
p8 <- plot_interventions(sdf, non_split_df, 'potable_new_infra')

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, top = "Events by type per state")




# Create Summary statistics
# Cumulative Sum SacMex census actions
# Mean vulnerability values

ggplot(sdf, aes(fill=sacmex_potable_maintenance_intervention_presence)) +
  geom_sf(size=0.05)
