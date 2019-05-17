context('megadapt')

library(megadaptr)
library(dplyr)

expect_between <- function(object, lb, ub, allow_empty = FALSE) {
  act <- quasi_label(rlang::enquo(object))

  if (allow_empty) {
    act$lb <- suppressWarnings(min(object))
    act$ub <- suppressWarnings(min(object))
  } else {
    act$lb <- min(object)
    act$ub <- max(object)
  }
  expect(
    act$lb >= lb || act$ub <= ub,
    sprintf('%s has range [%f, %f] not contained in [%f, %f]', act$lab, act$lb, act$ub, lb, ub)
  )

  invisible(act$val)
}

test_that('apply_data_changes updates original data with changed columns', {
  df <- data.frame(x=1:5, y=6:10, z=11:15, pk=1:5)
  changes <- data.frame(x=5:1, z=1:5, pk=c(1,3,2,4,5))

  new_df <- apply_data_changes(df, changes, join_columns=c('pk'='pk'))
  comp_df <- df %>%
    dplyr::select(-x, -z) %>%
    dplyr::inner_join(changes, by=c("pk"="pk")) %>%
    dplyr::arrange(pk)
  expect_equal(new_df$x, comp_df$x)
  expect_equal(new_df$z, comp_df$z)
  expect_equal(new_df$pk, comp_df$pk)
})

describe('a scenario cache', {
  runner <- function(p) {
    tibble(x=(1 + p$birth_rate - p$death_rate)^(1:10))
  }
  scenarios <- expand.grid(birth_rate=1:3*0.02, death_rate=1:2*0.01)
  cache_path <- 'test_data'

  it('can be saved to the file system', {
    cache <- create_scenario_cache(scenarios=scenarios, path=cache_path, runner=runner)
    expect_true(fs::file_exists('test_data/index.Rds'))
    expect_true(fs::file_exists('test_data/results/1.Rds'))
    expect_true(fs::file_access('test_data/results/6.Rds'))
  })

  it('can load a scenario index', {
    cache <- load_scenario_cache(cache_path)
    expect_equal(cache$path, cache_path)
    expect_equal(cache$index$birth_rate, scenarios$birth_rate)
    expect_equal(cache$index$death_rate, scenarios$death_rate)
  })

  it('can load a scenario result', {
    cache <- load_scenario_cache(cache_path)
    scenario <- load_scenario(cache, birth_rate == 0.02, death_rate == 0.01)
    expected_scenario <- runner(list(birth_rate = 0.02, death_rate = 0.01))
    expect_equal(scenario$x, expected_scenario$x)
  })

  it('can load multiple scenarios', {
    cache <- load_scenario_cache(cache_path)
    scenarios <- load_scenarios(cache, birth_rate == 0.02)
    expect_equal(nrow(scenarios), 2)
  })

  it('can be deleted', {
    delete_scenario_cache(cache_path)
  })
})

teardown({
  if (fs::dir_exists('test_data')) {
    fs::dir_delete('test_data')
  }
})

describe('sacmex infrastructure allocation (mutually exclusive)', {
  site_suitability <- tibble::tribble(
    ~ageb_id, ~A1, ~A2, ~A3, ~A4,
    1, 1, 0, 0, 0,
    2, 0, 3, 0, 0,
    3, 0, 0, 5, 0,
    4, 0, 0, 0, 7
    )

  it('is empty when budget is zero', {
    allocation <- determine_public_infrastructure_work_plan(
      site_suitability = site_suitability,
      budget = 0)
    expect_equal(nrow(allocation), 0)
  })

  it('includes the census blocks that in most need of work', {
    allocation <- determine_public_infrastructure_work_plan(
      site_suitability = site_suitability,
      budget = 2)
    expect_equal(allocation$ageb_id, c(4, 3))
    expect_equal(as.character(allocation$choice_name), c("A4", "A3"))
    expect_equal(allocation$max_choice_value, c(7, 5))
  })

  it('includes all census blocks if budget is greater than number of census blocks', {
    allocation <- determine_public_infrastructure_work_plan(
      site_suitability = site_suitability,
      budget = 5)
    expect_equal(allocation$ageb_id, c(4, 3, 2, 1))
    expect_equal(as.character(allocation$choice_name), c("A4", "A3", "A2", "A1"))
    expect_equal(allocation$max_choice_value, c(7, 5, 3, 1))
  })
})

describe('sacmex infracstructure allocation with separate potable, non potable budgets', {
  site_suitability <- tibble::tribble(
    ~ageb_id,
    ~non_potable_maintenance,
    ~non_potable_new_infrastructure,
    ~potable_maintenance,
    ~potable_new_infrastructure,
    1, 1, 0, 6, 0,
    2, 0, 3, 0, 0,
    3, 0, 0, 5, 0,
    4, 0, 5, 0, 7
  )

  it('is empty when both budgets are zero', {
    allocation <- determine_public_infrastructure_work_plan_separate_budgets(
      site_suitability = site_suitability,
      potable_water_budget = 0,
      non_potable_water_budget = 0)
    expect_equal(nrow(allocation), 0)
  })

  it('includes the census blocks that in most need of non potable work if non potable budget greater than zero', {
    allocation <- determine_public_infrastructure_work_plan_separate_budgets(
      site_suitability = site_suitability,
      potable_water_budget = 0,
      non_potable_water_budget = 3)
    expect_equal(allocation$ageb_id, c(4, 2, 1))
    expect_equal(
      as.character(allocation$choice_name),
      c("non_potable_new_infrastructure", "non_potable_new_infrastructure", "non_potable_maintenance"))
    expect_equal(allocation$max_choice_value, c(5, 3, 1))
  })

  it('includes the census blocks that in most need of potable work if potable budget greater than zero', {
    allocation <- determine_public_infrastructure_work_plan_separate_budgets(
      site_suitability = site_suitability,
      potable_water_budget = 3,
      non_potable_water_budget = 0)
    expect_equal(allocation$ageb_id, c(4, 1, 3))
    expect_equal(
      as.character(allocation$choice_name),
      c("potable_new_infrastructure", "potable_maintenance", "potable_maintenance"))
    expect_equal(allocation$max_choice_value, c(7, 6, 5))
  })

  it('includes all census blocks with a large enough budget', {
    allocation <- determine_public_infrastructure_work_plan_separate_budgets(
      site_suitability = site_suitability,
      potable_water_budget = 4,
      non_potable_water_budget = 4)
    expect_equal(nrow(allocation), 8)
  })
})

describe('a megadapt model', {
  if (Sys.getenv('R_INTEGRATION_TESTS') == '') {
    skip('Skipping integration tests')
  }

  megadapt <- build_megadapt_model(
    data_root_dir = system.file("rawdata", package = 'megadaptr', mustWork = TRUE),
    mental_model_file_names = list(
      potable_water_operator_limit = 'DF101215_GOV_AP_modificado_PNAS.limit.csv',
      non_potable_water_operator_limit = 'SACMEX_Drenaje_limit_SESMO.csv',
      overall_limit = 'I080316_OTR.limit.csv'
    ),
    params = list(n_steps = 2)
  )

  it('can have its parameters modified', {
    new_megadapt <- modify_megadapt_model(megadapt, list(new_infrastructure_effectiveness = 0.1))
    expect_equal(new_megadapt$params$new_infrastructure_effectiveness, 0.1)
  })

  describe('a megadapt model run', {
    results <- simulate_megadapt(megadapt)

    it('should have a water scarcity index within [0, 1]', {
      expect_between(results$water_scarcity_index, 0, 1)
    })

    it('should have a number of interventions in census block less than or equal to the number of years simulated', {
      expect_between(results$potable_water_system_intervention_count, 0, megadapt$params$n_steps)
      expect_between(results$non_potable_water_system_intervention_count, 0, megadapt$params$n_steps)
    })

    it('should have a percent with potable water within [0, 1]', {
      expect_between(results$potable_percent_lacking, 0, 1)
    })

    it('should have a sensitivity indices within [0, 1]', {
      expect_between(results$potable_water_sensitivity_index, 0, 1)
      expect_between(results$non_potable_water_sensitivity_index, 0, 1)
    })
  })
})

desribe('a file mental model update strategy', {
  mental_model <- file_mental_model_strategy(path, function(year, study_data) year > 2020)

  it('should return the first limit matrix if year less than or equal to 2020', {
    limit_matrix <- get_limit_matrix(mental_model, year, study_data)
    expect(limit_matrix == mental_model$limit_matrices[1])
  })

  it('should return the first limit matrix if year greater than 2020', {
    limit_matrix <- get_limit_matrix(mental_model, year, study_data)
    expect(limit_matrix == mental_model$limit_matrices[2])
  })
})
