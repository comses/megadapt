library(megadaptr)
library(dplyr)

study_data <- tibble::tibble(
  censusblock_id = c(1, 2),
  precipitation_volume = c(200000, 100000),
  precipitation_volume_mean = c(100000, 50000),
  runoff_volume = c(3, 2.5),
  runoff_volume_mean = c(2, 2),
  resident_reports_flooding_count =  c(0.12, 0.13),
  resident_reports_flooding_count_mean = c(0.15, 0.16),
  sewer_system_capacity = c(25, 25),
  sewer_system_capacity_max = c(50, 50)
)

describe('a flooding delta method fnss', {
  flooding_fnss <- flooding_delta_method_fnss_create()
  flooding_fnss_val <- call_fnss(flooding_fnss, study_data)$flooding_event_count[1]

  it('should decrease if capacity increases', {
    study_data$sewer_system_capacity <- 2 * study_data$sewer_system_capacity
    flooding_fnss_high_cap <- call_fnss(flooding_fnss, study_data)$flooding_event_count[1]
    expect_gt(flooding_fnss_val, flooding_fnss_high_cap)
  })

  it('should increase if precipitation increases', {
    study_data$precipitation_volume <- study_data$precipitation_volume + 50
    flooding_fnss_high_precip <- call_fnss(flooding_fnss, study_data)$flooding_event_count[1]
    expect_lt(flooding_fnss_val, flooding_fnss_high_precip)
  })

  it('should increase if runoff increases', {
    study_data$runoff_volume <- study_data$runoff_volume + 1
    flooding_fnss_high_runoff <- call_fnss(flooding_fnss, study_data)$flooding_event_count[1]
    expect_lte(flooding_fnss_val, flooding_fnss_high_runoff)
  })
})
