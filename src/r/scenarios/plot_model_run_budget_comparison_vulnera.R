library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)
library(plyr)




#path <- data_dir('censusblocks')
path <- "/Users/fidel/Documents/GitHub/megadapt/src/r/megadaptr/inst/rawdata/censusblocks"
sdf <- rgdal::readOGR(dsn = path,
                      layer = 'megadapt_wgs84',
                      stringsAsFactors = FALSE,
                      verbose = TRUE,
                      integer64 = 'warn.loss')

fortified <- fortify(sdf, region ="ageb_id")
fortified$censusblock_id <- as.character(fortified$id)
fortified$id <- fortified$censusblock_id




long_df<- new_results %>% filter(year %in% c(2020, 2030, 2040, 2050, 2060))
ll <- long_df %>% select(censusblock_id,
                         year,
                         household_potable_water_vulnerability)

ww <- reshape(ll, idvar = "censusblock_id", timevar = "year", direction = "wide")

map <- ggplot() +
  geom_map(data=fortified, map=fortified,
           aes(x=long, y=lat, map_id=censusblock_id),
           color="#2b2b2b", size=0.1, fill=NA) +
  geom_map(data=ll, map=fortified,
           aes(fill=household_potable_water_vulnerability, map_id=censusblock_id)) +
  scale_fill_viridis_c() +
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))+
  facet_grid(.~year) +
  labs(fill = 'Vulnerabilidad')





map
# Get data from split run

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=2222,
                  user="fidel",
                  host="localhost")

split_df <- dbGetQuery(conn,
"select censusblock_id::char(4),
  budget,
  avg(1 - household_potable_water_vulnerability) as vulnera_s,
  avg(1 - (household_sewer_vulnerability/907.6492)) as vulnera_f
from results_split_2
inner join params_split_2 as p on p.key = results_split_2.param_id
group by budget, censusblock_id;")


# Get data from non split run

non_split_df <- dbGetQuery(conn,
"select censusblock_id::char(4),
  budget,
  avg(1 - household_potable_water_vulnerability) as vulnera_s,
  avg(1 - (household_sewer_vulnerability/907.6492)) as vulnera_f
from results_non_split_2
inner join params_non_split_2 as p on p.key = results_non_split_2.param_id where year = 2060
group by budget, censusblock_id;")

long_names <- tibble::tribble(
  ~name, ~long_name,
  'vulnera_s', 'Vulnerabilidad Escasez',
  'vulnera_f', 'Vulnerabilidad Inundacion'
)

whole_df <- bind_rows(split_df %>% mutate(budget_type = 'Split'), non_split_df %>% mutate(budget_type = 'Non Split'))
long_df <- tidyr::gather(data = whole_df, key = statistic_name, value = statistic_value,
                         vulnera_s, vulnera_f) %>%
    mutate(statistic_value = statistic_value) %>%
    mutate(statistic_name = recode(statistic_name, !!! (long_names %>% tidyr::spread(name, long_name) %>% as.list)))

map_facet <- ggplot() +
  geom_map(data=fortified, map=fortified,
           aes(x=long, y=lat, map_id=censusblock_id),
           color="#2b2b2b", size=0.1, fill=NA) +
  geom_map(data=long_df, map=fortified,
           aes(fill=statistic_value, map_id=censusblock_id)) +
  scale_fill_viridis_c() +
  theme(strip.text.x = element_text(size = 40))+
  theme(strip.text.y = element_text(size = 40))+
  facet_grid(cols = vars(budget, budget_type), rows = vars(statistic_name)) +
  labs(fill = 'Vulnerabilidad')

ggsave('/Users/fidel/tawa/src/r/scenarios/budget_vulnera.png',
       plot = map_facet,
       device = 'png',
       width = 45,
       height = 15,
       units = 'in')


split_param_tbl <- tbl(conn, 'params_split_2')
split_result_tbl <- tbl(conn, 'results_split_2')

ponding_df <- split_result_tbl %>%
  inner_join(split_param_tbl, by = c('param_id'='key')) %>%
  filter(year == 2060) %>%
  group_by(censusblock_id, budget) %>%
  summarize(ponding_index = mean(ponding_index, na.rm = TRUE)) %>%
  collect()


# Create Summary statistics
# Cumulative Sum SacMex census actions
# Mean vulnerability values


