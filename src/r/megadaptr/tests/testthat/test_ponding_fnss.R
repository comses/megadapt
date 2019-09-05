library(megadaptr)
library(dplyr)

study_data <- tibble::tibble(
  censusblock_id = c(1, 2),
  precipitation_volume = c(200000, 100000),
  precipitation_volume_mean = c(100000, 50000),
  runoff_volume = c(3, 2.5),
  runoff_volume_mean = c(2, 2),
  resident_reports_ponding_count =  c(0.12, 0.13),
  resident_reports_ponding_count_mean = c(0.15, 0.16),
  sewer_system_capacity = c(25, 25),
  sewer_system_capacity_max = c(50, 50)
)

describe('a ponding delta method fnss', {
  ponding_fnss <- ponding_delta_method_fnss_create()
  ponding_fnss_df <- call_fnss(ponding_fnss, study_data)

  it('should decrease if capacity increases', {
    study_data$sewer_system_capacity <- 2 * study_data$sewer_system_capacity
    ponding_fnss_high_cap_df <- call_fnss(ponding_fnss, study_data)
    expect_gt(ponding_fnss_df$ponding_event_count[1], ponding_fnss_high_cap_df$ponding_event_count[1])
  })

  it('should increase if precipitation increases', {
    study_data$precipitation_volume <- study_data$precipitation_volume + 50
    ponding_fnss_high_precip_df <- call_fnss(ponding_fnss, study_data)
    expect_lt(ponding_fnss_df$ponding_event_count[1], ponding_fnss_high_precip_df$ponding_event_count[1])
  })

  it('should increase if runoff increases', {
    study_data$runoff_volume <- study_data$runoff_volume + 1
    ponding_fnss_high_runoff_df <- call_fnss(ponding_fnss, study_data)
    expect_lte(ponding_fnss_df$ponding_event_count[1], ponding_fnss_high_runoff_df$ponding_event_count[1])
  })
})

describe('a ponding config', {
  config <- ponding_config_create()

  it('can be used to construct a ponding model', {
    flooding_fnss <- ponding_deserialize(config)
    expect_s3_class(flooding_fnss, 'ponding_delta_method_fnss')
  })
})

