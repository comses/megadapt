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

cc_df <- dbGetQuery(conn,
"select censusblock_id::char(4),
climate_urban_scenarios_result.year,
climate__id,
sacmex__budget as budget,
sum(sacmex_sewer_maintenance_intervention_presence::int) as sewer_maintenance,
sum(sacmex_sewer_new_infrastructure_intervention_presence::int) as sewer_new_infra,
avg(household_sewer_sensitivity) as flooding_sensitibity,
avg(flooding_index::int) as flooding_exposure,
avg(household_sewer_vulnerability::int) as flooding_vulnerability
from climate_urban_scenarios_result
inner join climate_urban_scenarios_param as p on p.id = climate_urban_scenarios_result.param_id
group by budget, censusblock_id, climate_urban_scenarios_result.year, climate__id;")


# Get data from non split run

cc_extreme_df <- dbGetQuery(conn,
    "select censusblock_id::char(4),
    climate_urban_scenarios_extreme_result.year,
    climate__id,
    sacmex__budget as budget,
    sum(sacmex_sewer_maintenance_intervention_presence::int) as sewer_maintenance,
    sum(sacmex_sewer_new_infrastructure_intervention_presence::int) as sewer_new_infra,
    avg(household_sewer_sensitivity) as flooding_sensitibity,
    avg(flooding_index::int) as flooding_exposure,
    avg(household_sewer_vulnerability::int) as flooding_vulnerability
    from climate_urban_scenarios_extreme_result
    inner join climate_urban_scenarios_extreme_param as p on p.id = climate_urban_scenarios_extreme_result.param_id
    group by budget, censusblock_id, climate_urban_scenarios_extreme_result.year, climate__id;")




whole_df <- bind_rows(cc_df %>% mutate(extreme_events = 0), cc_extreme_df %>% mutate(extreme_events = 1))

scenarios_ids <- read.csv(file="/Users/fidel/Documents/GitHub/megadapt/src/r/megadaptr/inst/rawdata/climate_landuse_scenarios/index.csv", header=TRUE, sep=",")

scenarios_ids <- scenarios_ids %>% select(id,cc_scenario,urban_scenario)
names(scenarios_ids)[1] <- "climate__id"

whole_df <- left_join(whole_df,scenarios_ids,by="climate__id")

whole_df$flooding <- cut(whole_df$flooding_vulnerability,
                         breaks = c(0,0.065,0.125,0.25,0.5,1),
                         labels = c(1,2,3,4,5))




write.csv(whole_df,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/eventos_extremos/cc_extreme.csv", row.names = FALSE)

