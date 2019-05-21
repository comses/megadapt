library(megadaptr)
library(dplyr)

study_data <- tibble::tibble(
  ageb_id = 1,
  f_prec_v = 200000,
  non_potable_capacity = 25,
  f_esc = 3,
  prom_en =  0.12
)

describe('a ponding index fnss', {
  ponding_index_fnss <- ponding_index_fnss_create()

  it('should evaluate between 0 and 1', {
    ponding_index <- call_fnss(ponding_index_fnss, study_data)
    expect_between(ponding_index, 0, 1)
  })
})

describe('a ponding delta method fnss', {
  ponding_fnss <- ponding_delta_method_fnss_create()

  it('should decrease if capacity increases', {
    study_data$non_potable_capacity <- 50
    ponding_fnss_high_cap <- call_fnss(ponding_fnss, study_data)
    expect_lt(ponding_fnss, ponding_fnss_high_cap)
  })

  it('should increase if precipitation increases', {
    study_data$f_prec_v <- 300000
    ponding_fnss_high_precip <- call_fnss(ponding_fnss, study_data)
    expect_gt(ponding_fnss, ponding_fnss_high_precip)
  })

  it('should increase if runoff increases', {
    study_data$f_esc <- 0.5
    ponding_fnss_high_runoff <- call_fnss(ponding_fnss, study_data)
    expect_gt(ponding_fnss, ponding_fnss_high_runoff)
  })
})
