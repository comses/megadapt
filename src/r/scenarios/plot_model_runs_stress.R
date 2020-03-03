library(ggplot2)
library("RPostgreSQL")
library("DBI")
library(dplyr)
library(gridExtra)
library(plyr)
library(RColorBrewer)



#path <- data_dir('censusblocks')
plots_folder <- "/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/stressing_model/equidista"
path <- "/Users/fidel/Documents/GitHub/megadapt/src/r/megadaptr/inst/rawdata/censusblocks"
sdf <- rgdal::readOGR(dsn = path,
                      layer = 'megadapt_wgs84',
                      stringsAsFactors = FALSE,
                      verbose = TRUE,
                      integer64 = 'warn.loss')

fortified <- fortify(sdf, region ="ageb_id")
fortified$censusblock_id <- as.character(fortified$id)
fortified$id <- fortified$censusblock_id

results_df <- read.csv("/Users/fidel/Dropbox (LANCIS)/fserrano/megadapt/stressing_model/ss_equidista.csv")
results_df <- results_df %>% filter(year == 2060)
results_df <- results_df %>% select(censusblock_id,budget,budget_split,stress,scarcity_vulnerability_cat, scaricity_exposure_cat)

myPalette <- colorRampPalette(rev(brewer.pal(5, "RdYlGn")))
sc <- scale_fill_gradientn(colours = myPalette(5), limits=c(1, 5))

map <- ggplot() +
  geom_map(data=fortified, map=fortified,
           aes(x=long, y=lat, map_id=censusblock_id),
           color="#2b2b2b", size=0.1, fill=NA) +
  geom_map(data=results_df, map=fortified,
           aes(fill=scaricity_exposure_cat, map_id=censusblock_id)) +
  sc +
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))+
  facet_grid(budget + budget_split ~ stress) +
  labs(fill = 'Exposción') +
  ggtitle("Exposición escasez (equidistante)")



ggsave(paste0(plots_folder,'/exposicion_escasez.png'),
       plot = map,
       device = 'png',
       width = 16,
       height = 16,
       units = 'in')


map <- ggplot() +
  geom_map(data=fortified, map=fortified,
           aes(x=long, y=lat, map_id=censusblock_id),
           color="#2b2b2b", size=0.1, fill=NA) +
  geom_map(data=results_df, map=fortified,
           aes(fill=scarcity_vulnerability_cat, map_id=censusblock_id)) +
  sc +
  theme(strip.text.x = element_text(size = 20))+
  theme(strip.text.y = element_text(size = 20))+
  facet_grid(budget + budget_split ~ stress) +
  labs(fill = 'Vulnerabilidad') +
  ggtitle("Vulnerabilidad escasez (equidistante)")



ggsave(paste0(plots_folder,'/vulnerabilidad_escasez.png'),
       plot = map,
       device = 'png',
       width = 16,
       height = 16,
       units = 'in')


