library(megadaptr)
library(dplyr)

describe('a ponding index fnss', {
  study_data <- tibble::tibble(
    ageb_id = 1,
    f_prec_v = 200000,
    non_potable_capacity = 25,
    f_esc = 3,
    prom_en =  0.12
  )
  ponding_index_fnss <- ponding_index_fnss_create()

  it('should evaluate between 0 and 1', {
    ponding_index <- call_fnss(ponding_index_fnss, study_data)
    expect_between(ponding_index, 0, 1)
  })
})
