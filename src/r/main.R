source('r/setup.R')
source('r/example.R')

require(magrittr)
require(dplyr)

set.seed(1000)

megadapt <- example()
new_results <- simulate_megadapt(megadapt)