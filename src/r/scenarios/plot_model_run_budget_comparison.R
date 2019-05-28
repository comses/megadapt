library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)
library(plyr)




#path <- data_dir('censusblocks')
path <- "/Users/fidel/tawa/src/r/megadaptr/inst/rawdata/censusblocks"
sdf <- rgdal::readOGR(dsn = path,
                      layer = 'megadapt_wgs84',
                      stringsAsFactors = FALSE,
                      verbose = TRUE,
                      integer64 = 'warn.loss')

fortified <- fortify(sdf, region ="ageb_id")
fortified$censusblock_id <- as.character(fortified$id)
fortified$id <- fortified$censusblock_id


# Get data from split run

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=2222,
                  user="fidel",
                  host="localhost")

split_df <- dbGetQuery(conn, "SELECT censusblock_id::char(4),
sum(sacmex_potable_maintenance_intervention_presence::int) as potable_maintenance,
sum(sacmex_potable_new_infrastructure_intervention_presence::int) as potable_new_infra,
sum(sacmex_sewer_maintenance_intervention_presence::int) as sewer_maintenance,
sum(sacmex_sewer_new_infrastructure_intervention_presence::int) as sewer_new_infra
from results_split_2 group by censusblock_id;")


# Get data from non split run

non_split_df <- dbGetQuery(conn, "SELECT censusblock_id::char(4),
sum(sacmex_potable_maintenance_intervention_presence::int) as potable_maintenance,
sum(sacmex_potable_new_infrastructure_intervention_presence::int) as potable_new_infra,
sum(sacmex_sewer_maintenance_intervention_presence::int) as sewer_maintenance,
sum(sacmex_sewer_new_infrastructure_intervention_presence::int) as sewer_new_infra
from results_non_split_2 group by censusblock_id;")

long_names <- tibble::tribble(
  ~name, ~long_name,
  'potable_maintenance', 'Potable Maintenance',
  'potable_new_infra', 'Potable New Infra',
  'sewer_maintenance', 'Sewer Maintenance',
  'sewer_new_infra', 'Sewer New Infra'
)

whole_df <- bind_rows(split_df %>% mutate(budget = 'Split'), non_split_df %>% mutate(budget = 'Non Split'))
long_df <- tidyr::gather(data = whole_df, key = statistic_name, value = statistic_value, 
                         potable_maintenance, potable_new_infra, sewer_maintenance, sewer_new_infra) %>%
    mutate(statistic_value = statistic_value / 800) %>%
    mutate(statistic_name = recode(statistic_name, !!! (long_names %>% tidyr::spread(name, long_name) %>% as.list)))
  
ggplot() +
  geom_map(data=fortified, map=fortified,
           aes(x=long, y=lat, map_id=censusblock_id),
           color="#2b2b2b", size=0.1, fill=NA) +
  geom_map(data=long_df, map=fortified,
           aes(fill=statistic_value, map_id=censusblock_id)) +
  scale_fill_viridis_c() +
  facet_grid(rows = vars(budget), cols = vars(statistic_name)) +
  labs(fill = 'Intervention Count')

plot_interventions <- function(polygons, df, col_name) {
  fortified_d <- merge(polygons, df, by = 'censusblock_id')


  ggplot() +
    geom_polygon(data = long_df, aes_string(fill = statistic_value, 
                                            x = "long", 
                                            y = "lat", 
                                            group = "group")) +
    facet_grid(rows = budget, cols = statistic_name)
}


p1 <- plot_interventions(fortified, split_df, 'sewer_maintenance')
p2 <- plot_interventions(fortified, split_df, 'sewer_new_infra')
p3 <- plot_interventions(fortified, split_df, 'potable_maintenance')
p4 <- plot_interventions(fortified, split_df, 'potable_new_infra')
p5 <- plot_interventions(fortified, non_split_df, 'sewer_maintenance')
p6 <- plot_interventions(fortified, non_split_df, 'sewer_new_infra')
p7 <- plot_interventions(fortified, non_split_df, 'potable_maintenance')
p8 <- plot_interventions(fortified, non_split_df, 'potable_new_infra')


png(file="facet_map.png",
    width=1980, height=800)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, top = "Intervention sum")
dev.off()





# Create Summary statistics
# Cumulative Sum SacMex census actions
# Mean vulnerability values


