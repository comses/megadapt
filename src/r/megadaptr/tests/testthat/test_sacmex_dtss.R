library(megadaptr)

site_suitability <- tibble::tribble(
  ~ censusblock_id,
  ~ non_potable_maintenance,
  ~ non_potable_new_infrastructure,
  ~ potable_maintenance,
  ~ potable_new_infrastructure,
  1, 1, 0, 6, 0,
  2, 0, 3, 0, 0,
  3, 0, 0, 5, 0,
  4, 0, 5, 0, 7
)

describe('sacmex infracstructure allocation with separate potable, non potable budgets',
          {
           it('is empty when both budgets are zero', {
             allocation <-
               sacmex_work_plan_sewer_potable_split(
                 site_suitability = site_suitability,
                 budget = list(potable_water = 0, sewer = 0)
               )
             expect_equal(nrow(allocation), 0)
           })

           it(
             'includes the census blocks that in most need of non potable work if non potable budget greater than zero',
             {
               allocation <-
                 sacmex_work_plan_sewer_potable_split(
                   site_suitability = site_suitability,
                   budget = list(potable_water = 0, sewer = 3)
                 )
               expect_equal(allocation$censusblock_id, c(4, 2, 1))
               expect_equal(
                 as.character(allocation$choice_name),
                 c(
                   "non_potable_new_infrastructure",
                   "non_potable_new_infrastructure",
                   "non_potable_maintenance"
                 )
               )
               expect_equal(allocation$max_choice_value, c(5, 3, 1))
             }
           )

           it(
             'includes the census blocks that in most need of potable work if potable budget greater than zero',
             {
               allocation <-
                 sacmex_work_plan_sewer_potable_split(
                   site_suitability = site_suitability,
                   budget = list(potable_water = 3, sewer = 0)
                 )
               expect_equal(allocation$censusblock_id, c(4, 1, 3))
               expect_equal(
                 as.character(allocation$choice_name),
                 c(
                   "potable_new_infrastructure",
                   "potable_maintenance",
                   "potable_maintenance"
                 )
               )
               expect_equal(allocation$max_choice_value, c(7, 6, 5))
             }
           )

           it('includes all census blocks with a large enough budget', {
             allocation <-
               sacmex_work_plan_sewer_potable_split(
                 site_suitability = site_suitability,
                 budget = list(potable_water = 4, sewer = 4)
               )
             expect_equal(nrow(allocation), 8)
           })
         })


describe('a split infrastructure allocation', {
  it('should invest in all census blocks if budget if large enough', {
    allocation <-
      sacmex_work_plan_separate_action_budgets(
        site_suitability = site_suitability,
        budget = list(
          potable_water_new_infrastructure = 10,
          potable_water_maintenance = 500,
          sewer_water_new_infrastructure = 4,
          sewer_water_maintenance = 5
        )
      )
    expect_equal(nrow(allocation), 16)
  })

  it('should invest in no census blocks if budget is zero', {
    allocation <-
      sacmex_work_plan_separate_action_budgets(
        site_suitability = site_suitability,
        budget = list(
          potable_water_new_infrastructure = 0,
          potable_water_maintenance = 0,
          sewer_water_new_infrastructure = 0,
          sewer_water_maintenance = 0
        )
      )
    expect_equal(nrow(allocation), 0)
  })

  it('includes most in need census blocks if budget between 0 and max', {
    allocation <-
      sacmex_work_plan_separate_action_budgets(
        site_suitability = site_suitability,
        budget = list(
          potable_water_new_infrastructure = 2,
          potable_water_maintenance = 2,
          sewer_water_new_infrastructure = 1,
          sewer_water_maintenance = 1
        )
      )
    expect_equal(nrow(allocation), 6)
  })
})

describe('a split budget', {
  make_weight_vec <- function(x) {
    names(x) <- c('Mantenimiento', 'Nueva_infraestructura')
    x
  }
  mental_models = list(alternative_weights = list(s = make_weight_vec(c(0.2, 0.8)),
                                                  d = make_weight_vec(c(0.1, 0.9))))
  sewer_budget <- 500
  potable_water_budget <- 1000

  it('should partition the budget according to the mental weights', {
    budget <-
      sacmex_get_budget_from_mental_model(
        sewer_budget = sewer_budget,
        potable_water_budget = potable_water_budget,
        mental_models = mental_models
      )
    expect_equal(budget$potable_water_new_infrastructure, 800)
    expect_equal(budget$potable_water_maintenance, 200)
    expect_equal(budget$sewer_water_new_infrastructure, 450)
    expect_equal(budget$sewer_water_maintenance, 50)
  })
})
