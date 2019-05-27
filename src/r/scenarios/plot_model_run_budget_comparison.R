library(ggplot2)

sdf <- sf::st_read('inst/rawdata/censusblocks/megadapt_wgs84_v5.gpkg') %>%
  dplyr::rename(geometry=geom)

# Get data from split run

# Get data from non split run

# Union the two datasets together

# Create Summary statistics
# Cumulative Sum SacMex census actions
# Mean vulnerability values


ggplot(sdf, aes(fill=sewer_system_max_capacity)) +
  geom_sf(size=0.05) +
  facet_wrap(system, budget_type)
