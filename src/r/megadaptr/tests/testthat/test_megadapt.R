library(megadaptr)
library(dplyr)

test_that('apply_data_changes updates original data with changed columns',
          {
            df <- data.frame(
              x = 1:5,
              y = 6:10,
              z = 11:15,
              pk = 1:5
            )
            changes <- data.frame(x = 5:1,
                                  z = 1:5,
                                  pk = c(1, 3, 2, 4, 5))

            new_df <-
              apply_data_changes(df, changes, join_columns = c('pk' = 'pk'))
            comp_df <- df %>%
              dplyr::select(-x,-z) %>%
              dplyr::inner_join(changes, by = c("pk" = "pk")) %>%
              dplyr::arrange(pk)
            expect_equal(new_df$x, comp_df$x)
            expect_equal(new_df$z, comp_df$z)
            expect_equal(new_df$pk, comp_df$pk)
          })

describe('a megadapt model', {
  if (Sys.getenv('R_INTEGRATION_TESTS') == '') {
    skip('Skipping integration tests')
  }

  megadapt <- suppressWarnings(
    megadapt_create(
      params_create(),
      sacmex_fnss_creator = sacmex_fnss_create,
      mental_models = mental_model_constant_strategies(),
      flooding_fnss = flooding_delta_method_fnss_create(),
      ponding_fnss = ponding_delta_method_fnss_create()
    )
  )

  it('can have its parameters modified', {
    new_megadapt <- suppressWarnings(
      megadapt_create(
        params_create(new_infrastructure_effectiveness = 0.1),
        sacmex_fnss_creator = sacmex_fnss_create,
        mental_models = mental_model_constant_strategies(),
        flooding_fnss = flooding_delta_method_fnss_create(),
        ponding_fnss = ponding_delta_method_fnss_create(),
        study_area = megadapt$study_area
      ))
    expect_equal(new_megadapt$sacmex_fnss$params$new_infrastructure_effectiveness,
                 0.1)
  })

  describe('a megadapt model run', {
    results <- simulate(megadapt)

    it('should have a water scarcity index within [0, 1]', {
      expect_between(results$scarcity_index_exposure, 0, 1)
      expect_between(results$scarcity_index_sensitivity, 0, 1)
    })

    it('should have a percent with potable water within [0, 1]', {
      expect_between(results$household_potable_system_lacking_percent, 0, 1)
    })

    it('should have a sensitivity indices within [0, 1]', {
      expect_between(results$household_potable_water_sensitivity, 0, 1)
      expect_between(results$household_sewer_sensitivity, 0, 1)
    })
  })
})
