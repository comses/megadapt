library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)
library(plyr)


# Get data from split run

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=2222,
                  user="fidel",
                  host="localhost")


# avg(non_potable_maintenance) as flooding_maintenance_distance,
# avg(non_potable_new_infrastructure) as flooding_new_infra_distance,
# avg(potable_maintenance) as scarcity_maintenance_distance,
# avg(potable_new_infrastructure) as scarcity_new_infra_distance,
# sum(sacmex_potable_maintenance_intervention_presence::int) as scarcity_maintenance_intervention,
# sum(sacmex_potable_new_infrastructure_intervention_presence::int) as scarcity_new_infra_intervention,
# sum(sacmex_sewer_maintenance_intervention_presence::int) as flooding_maintenance_intervention,
# sum(sacmex_sewer_new_infrastructure_intervention_presence::int) as flooding_new_infra_intervention



stressing_scenario <- function(scenario_name){
  paste0("select censusblock_id::char(4),",scenario_name,"_result.year,
         sacmex__budget as budget,
         sacmex__constructor as budget_split,
         climate__id as climate_id,
         avg(1 - scarcity_index_exposure) as scarcity_exposure,
         avg(household_potable_water_vulnerability) as scarcity_vulnerability,
         avg(1 - flooding_index) as flooding_exposure,
         avg(household_sewer_vulnerability) as flooding_vulnerability
         from ",scenario_name,"_result
         inner join ",scenario_name,"_param as p on p.id = ",scenario_name,"_result.param_id
         group by climate_id, budget_split, budget, censusblock_id, ",scenario_name,"_result.year;")
}

whole_df <- dbGetQuery(conn,stressing_scenario("ss_base_n_cs"))
# ss_asentamientos <- dbGetQuery(conn,stressing_scenario("ss_asentamientos"))
# ss_increm_cutza <- dbGetQuery(conn,stressing_scenario("ss_increm_cutza"))
# ss_mejora_efi <- dbGetQuery(conn,stressing_scenario("ss_mejora_efi"))
# ss_reduc_cutza <- dbGetQuery(conn,stressing_scenario("ss_reduc_cutza"))
# ss_reduc_agua <- dbGetQuery(conn,stressing_scenario("ss_reduc_agua"))

# whole_df <- bind_rows(ss_base %>% mutate(stress = "base"),
#                       ss_asentamientos %>% mutate(stress = "asentamientos"),
#                       ss_increm_cutza %>% mutate(stress = "increm_cutza"),
#                       ss_mejora_efi %>% mutate(stress = "mejora_efi"),
#                       ss_reduc_cutza %>% mutate(stress = "reduc_cutza"),
#                       ss_reduc_agua %>% mutate(stress = "reduc_agua"))

# whole_df$scarcity_vulnerability_cat <- cut(whole_df$scarcity_vulnerability,
#                          breaks = c(0,0.065,0.125,0.25,0.5,1),
#                          labels = c(1,2,3,4,5))

whole_df$scarcity_exposure_cat_15 <- cut(whole_df$scarcity_exposure,
                                       breaks = c(0,0.198,0.296,0.444,0.667,1),
                                       labels = c(1,2,3,4,5))

whole_df$scarcity_exposure_cat <- cut(whole_df$scarcity_exposure,
                                         breaks = c(0,0.2,0.4,0.6,0.8,1),
                                         labels = c(1,2,3,4,5))

whole_df$scarcity_vulnerability_cat <- cut(whole_df$scarcity_vulnerability,
                                           breaks = c(0,0.2,0.4,0.6,0.8,1),
                                           labels = c(1,2,3,4,5))

whole_df$scarcity_vulnerability_cat_15 <- cut(whole_df$scarcity_vulnerability,
                                           breaks = c(0,0.198,0.296,0.444,0.667,1),
                                           labels = c(1,2,3,4,5))


whole_df$flooding_exposure_cat_15 <- cut(whole_df$flooding_exposure,
                                         breaks = c(0,0.198,0.296,0.444,0.667,1),
                                         labels = c(1,2,3,4,5))

whole_df$flooding_exposure_cat <- cut(whole_df$flooding_exposure,
                                      breaks = c(0,0.2,0.4,0.6,0.8,1),
                                      labels = c(1,2,3,4,5))

whole_df$flooding_vulnerability_cat <- cut(whole_df$flooding_vulnerability,
                                           breaks = c(0,0.2,0.4,0.6,0.8,1),
                                           labels = c(1,2,3,4,5))

whole_df$flooding_vulnerability_cat_15 <- cut(whole_df$flooding_vulnerability,
                                              breaks = c(0,0.198,0.296,0.444,0.667,1),
                                              labels = c(1,2,3,4,5))





write.csv(whole_df,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/stressing_model/ss_base_test.csv", row.names = FALSE)

