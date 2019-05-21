library(megadaptr)
library(dplyr)

study_data <- tibble::tibble(
  censusblock_id = 1,
  precipitation_volume = 200000,
  precipitation_volume_mean = 100000,
  runoff_volume = 3,
  runoff_volume_mean = 2,
  resident_reports_ponding_count =  0.12,
  resident_reports_ponding_count_mean = 0.15,
  sewer_system_capacity = 25,
  sewer_system_capacity_initial = 20
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
  ponding_fnss_val <- call_fnss(ponding_fnss, study_data)

  it('should decrease if capacity increases', {
    study_data$sewer_system_capacity <- 2 * study_data$sewer_system_capacity
    ponding_fnss_high_cap <- call_fnss(ponding_fnss, study_data)
    expect_gt(ponding_fnss_val, ponding_fnss_high_cap)
  })

  it('should increase if precipitation increases', {
    study_data$precipitation_volume <- study_data$precipitation_volume + 50
    ponding_fnss_high_precip <- call_fnss(ponding_fnss, study_data)
    expect_lt(ponding_fnss_val, ponding_fnss_high_precip)
  })

  it('should increase if runoff increases', {
    study_data$runoff_volume <- study_data$runoff_volume + 1
    ponding_fnss_high_runoff <- call_fnss(ponding_fnss, study_data)
    expect_lt(ponding_fnss_val, ponding_fnss_high_runoff)
  })
})
