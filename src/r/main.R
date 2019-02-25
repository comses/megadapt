source('r/setup.R')
source('r/example.R')
source('r/verification.R')

require(magrittr)
require(dplyr)

set.seed(1000)

megadapt <- example()
new_results <- simulate_megadapt(megadapt)
old_results <- readRDS('../data/comparison.rds')
comparison <- compare_results(new_results, old_results,
                              c('AGEB_ID', 'municipio', 'time_sim', 'year_sim', 'month_sim'))