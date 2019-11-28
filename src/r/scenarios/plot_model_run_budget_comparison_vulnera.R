library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)
library(plyr)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname = "megadapt",
                  port=2222,
                  user="fidel",
                  host="localhost")




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



for(p_id in 1:500) {


  results_df <- dbGetQuery(conn,
                           paste0("select censusblock_id::char(4),
                           year,
                           household_potable_water_vulnerability,
                           household_sewer_vulnerability
                           from efectiveness_variation_result where param_id = ",p_id,";"))

  long_df <- results_df %>% filter(year %in% c(2021, 2030, 2040, 2050, 2060))
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



  ggsave(paste0('/Users/fidel/Desktop/escasez/escasez_',p_id,'.png'),
         plot = map,
         device = 'png',
         width = 10,
         height = 5,
         units = 'in')

  long_df <- results_df %>% filter(year %in% c(2021, 2030, 2040, 2050, 2060))
  ll <- long_df %>% select(censusblock_id,
                           year,
                           household_sewer_vulnerability)

  ww <- reshape(ll, idvar = "censusblock_id", timevar = "year", direction = "wide")

  map <- ggplot() +
    geom_map(data=fortified, map=fortified,
             aes(x=long, y=lat, map_id=censusblock_id),
             color="#2b2b2b", size=0.1, fill=NA) +
    geom_map(data=ll, map=fortified,
             aes(fill=household_sewer_vulnerability, map_id=censusblock_id)) +
    scale_fill_viridis_c() +
    theme(strip.text.x = element_text(size = 20))+
    theme(strip.text.y = element_text(size = 20))+
    facet_grid(.~year) +
    labs(fill = 'Vulnerabilidad')



  ggsave(paste0('/Users/fidel/Desktop/inundacion/inundacion_',p_id,'.png'),
         plot = map,
         device = 'png',
         width = 10,
         height = 5,
         units = 'in')

}

# Get data from split run



