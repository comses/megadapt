library(megadaptr)
library(dplyr)

study_data <- tibble::tibble(
  censusblock_id = 1,
  precipitation_volume = 200000,
  precipitation_volume_mean = 100000,
  runoff_volume = 3,
  runoff_volume_mean = 2,
  resident_reports_flooding_count =  0.12,
  resident_reports_flooding_count_mean = 0.15,
  sewer_system_capacity = 25,
  sewer_system_capacity_initial = 20
)

# describe('a flooding index fnss', {
#   study_data <- tibble::tibble(
#     ageb_id = 1,
#     f_prec_v = 200000,
#     non_potable_capacity = 25,
#     f_esc = 3,
#     inunda =  0.25
#   )
#   flooding_index_fnss <- flooding_index_fnss_create()
#
#   it('should evaluate between 0 and 1', {
#     flooding_index <- flooding_index_fnss()
#   })
# })

describe('a flooding delta method fnss', {
  flooding_fnss <- flooding_delta_method_fnss_create()
  flooding_fnss_val <- call_fnss(flooding_fnss, study_data)$flooding_index

  it('should decrease if capacity increases', {
    study_data$sewer_system_capacity <- 2 * study_data$sewer_system_capacity
    flooding_fnss_high_cap <- call_fnss(flooding_fnss, study_data)$flooding_index
    expect_gt(flooding_fnss_val, flooding_fnss_high_cap)
  })

  it('should increase if precipitation increases', {
    study_data$precipitation_volume <- study_data$precipitation_volume + 50
    flooding_fnss_high_precip <- call_fnss(flooding_fnss, study_data)$flooding_index
    expect_lt(flooding_fnss_val, flooding_fnss_high_precip)
  })

  it('should increase if runoff increases', {
    study_data$runoff_volume <- study_data$runoff_volume + 1
    flooding_fnss_high_runoff <- call_fnss(flooding_fnss, study_data)$flooding_index
    expect_lt(flooding_fnss_val, flooding_fnss_high_runoff)
  })
})
