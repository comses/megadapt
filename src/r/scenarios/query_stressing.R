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

stress_df <- dbGetQuery(conn,
"select censusblock_id::char(4),
stressing_base_result.year,
sacmex__budget as budget,
sacmex__constructor as budget_split,
avg(scarcity_index_exposure) as scarcity_exposure,
avg(household_potable_water_vulnerability) as scarcity_vulnerability
from stressing_base_result
inner join stressing_base_param as p on p.id = stressing_base_result.param_id
group by budget_split, budget, censusblock_id, stressing_base_result.year;")


# Get data from non split run

stress_extreme_df <- dbGetQuery(conn,
    "select censusblock_id::char(4),
    stressing_extreme_result.year,
    sacmex__budget as budget,
    sacmex__constructor as budget_split,
    avg(scarcity_index_exposure) as scarcity_exposure,
    avg(household_potable_water_vulnerability) as scarcity_vulnerability
    from stressing_extreme_result
    inner join stressing_extreme_param as p on p.id = stressing_extreme_result.param_id
    group by budget_split, budget, censusblock_id, stressing_extreme_result.year;")




whole_df <- bind_rows(stress_df %>% mutate(less_water = 0), stress_extreme_df %>% mutate(less_water = 1))

# scenarios_ids <- read.csv(file="/Users/fidel/Documents/GitHub/megadapt/src/r/megadaptr/inst/rawdata/climate_landuse_scenarios/index.csv", header=TRUE, sep=",")
#
# scenarios_ids <- scenarios_ids %>% select(id,cc_scenario,urban_scenario)
# names(scenarios_ids)[1] <- "climate__id"
#
# whole_df <- left_join(whole_df,scenarios_ids,by="climate__id")

whole_df$scarcity_vulnerability_cat <- cut(whole_df$scarcity_vulnerability,
                         breaks = c(0,0.065,0.125,0.25,0.5,1),
                         labels = c(1,2,3,4,5))





whole_df$scaricity_exposure_cat <- cut(whole_df$scarcity_exposure,
                          breaks = c(0,0.5,0.75,0.875,0.935,1),
                          labels = c(5,4,3,2,1))




write.csv(whole_df,"/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/stressing_model/less_water.csv", row.names = FALSE)

