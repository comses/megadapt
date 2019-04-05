context('model run')

library(megadaptr)
source('example.R')

expect_between <- function(object, lb, ub) {
  act <- quasi_label(rlang::enquo(object))

  act$lb <- min(object)
  act$ub <- max(object)
  expect(
    act$lb >= lb || act$ub <= ub,
    sprintf('%s has range [%d, %d] not contained in [%d, %d]', act$lab, act$lb, act$ub, lb, ub)
  )

  invisible(act$val)
}

model <- build_megadapt_model()
results <- simulate_megadapt(model)

describe('a megadapt model run', {
  it('should have a water scarcity index within [0, 1]', {
    expect_between(results$water_scarcity_index, 0, 1)
  })

  it('should have a number of interventions in census block less than or equal to the number of years simulated', {
    expect_between(results$potable_water_system_intervention_count, 0, model$params$n_steps)
    expect_between(results$non_potable_water_system_intervention_count, 0, model$params$n_steps)
  })

  it('should have a percent with potable water within [0, 1]', {
    expect_between(results$percent_without_potable_water, 0, 1)
  })

  it('should have a sensitivity indices within [0, 1]', {
    expect_between(results$potable_water_sensitivity_index, 0, 1)
    expect_between(results$non_potable_water_sensitivity_index, 0, 1)
  })
})
