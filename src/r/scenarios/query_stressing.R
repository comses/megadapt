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

stressing_scenario <- function(scenario_name){
  paste0("select censusblock_id::char(4),",scenario_name,"_result.year,
         sacmex__budget as budget,
         sacmex__constructor as budget_split,
         avg(scarcity_index_exposure) as scarcity_exposure,
         avg(household_potable_water_vulnerability) as scarcity_vulnerability
         from ",scenario_name,"_result
         inner join ",scenario_name,"_param as p on p.id = ",scenario_name,"_result.param_id
         group by budget_split, budget, censusblock_id, ",scenario_name,"_result.year;")
}

base <- dbGetQuery(conn,stressing_scenario("stressing_base"))
extreme <- dbGetQuery(conn,stressing_scenario("stressing_extreme"))
abundance <- dbGetQuery(conn,stressing_scenario("stressing_abundance"))
asentamientos <- dbGetQuery(conn,stressing_scenario("stressing_asentamientos"))
no_extraction <- dbGetQuery(conn,stressing_scenario("stressing_no_extraction"))

whole_df <- bind_rows(base %>% mutate(stress = "base"),
                      extreme %>% mutate(stress = "sequia"),
                      abundance %>% mutate(stress = "abundancia"),
                      asentamientos %>% mutate(stress = "asentamientos"),
                      no_extraction %>% mutate(stress = "sin_extraccion"))

# whole_df$scarcity_vulnerability_cat <- cut(whole_df$scarcity_vulnerability,
#                          breaks = c(0,0.065,0.125,0.25,0.5,1),
#                          labels = c(1,2,3,4,5))

whole_df$scarcity_vulnerability_cat <- cut(whole_df$scarcity_vulnerability,
                                           breaks = c(0,0.2,0.4,0.6,0.8,1),
                                           labels = c(1,2,3,4,5))





# whole_df$scaricity_exposure_cat <- cut(whole_df$scarcity_exposure,
#                           breaks = c(0,0.5,0.75,0.875,0.935,1),
#                           labels = c(5,4,3,2,1))

whole_df$scaricity_exposure_cat <- cut(whole_df$scarcity_exposure,
                                       breaks = c(0,0.2,0.4,0.6,0.8,1),
                                       labels = c(5,4,3,2,1))




write.csv(whole_df,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/stressing_model/stressing_scenarios_equidista.csv", row.names = FALSE)

