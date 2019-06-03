library(megadaptr)

describe('a coupled mental model', {
  study_data <- tibble::tibble(
    resident_reports_ponding_count_mean = 1,
    resident_reports_flooding_count_mean = 1,
    ponding_index = 0.5,
    flooding_index = 0.5,
    runoff_presence = 1
  )
  uwm <- read_unweighted_matrix(data_dir('mental_models/sewer_water_sacmex_unweighted_stage1.csv'))
  w <- mental_model_update_risks(unweighted_matrix_meta = uwm, study_data = study_data)

  it('should weight runoff more if runoff is increased', {
    study_data$ponding_index = 0.9
    w1 <- mental_model_update_risks(unweighted_matrix_meta = uwm, study_data = study_data)
    expect_gt(w1['Encharcamientos', 'Escurrimiento'], w['Encharcamientos', 'Escurrimiento'])
  })
})

describe('a file mental model update strategy', {
  paths <- c(system.file("rawdata/mental_models/potable_water_sacmex_unweighted_stage1.csv", package = 'megadaptr', mustWork = TRUE),
             system.file("rawdata/mental_models/potable_water_sacmex_unweighted_stage2.csv", package = 'megadaptr', mustWork = TRUE))
  cluster <- read_cluster_matrix(system.file('rawdata/mental_models/potable_water_cluster_sacmex.csv', package = 'megadaptr', mustWork = TRUE))
  mental_model <- file_mental_model_strategy(paths = paths, cluster = cluster, limit_df_picker = function(year, study_data) {
    if (year <= 2020) {
      return(1)
    } else {
      return(2)
    }
  })

  it('should return the first limit df if year less than or equal to 2020', {
    limit_df <- get_limit_df(mental_model, 1990, NULL)
    expect(all(limit_df == mental_model$limit_dfs[[1]]), "Picked wrong limit df")
  })

  it('should return the first limit df if year greater than 2020', {
    limit_df <- get_limit_df(mental_model, 2021, NULL)
    expect(all(limit_df == mental_model$limit_dfs[[2]]), "Picked wrong limit df")
  })
})

describe('a file mental model update strategy', {
  path <- system.file("rawdata/mental_models/potable_water_sacmex_unweighted_stage1.csv", package = 'megadaptr', mustWork = TRUE)
  cluster <- read_cluster_matrix(system.file('rawdata/mental_models/potable_water_cluster_sacmex.csv', package = 'megadaptr', mustWork = TRUE))
  mental_model <- mental_model_file_constant_strategy_create(path = path, cluster = cluster)

  it('should return the only limit df if year less than or equal to 2020', {
    limit_df <- get_limit_df(mental_model, 1990, NULL)
    expect(all(limit_df == mental_model$limit_df), "Picked wrong limit df")
  })

  it('should return the only limit df if year greater than 2020', {
    limit_df <- get_limit_df(mental_model, 2021, NULL)
    expect(all(limit_df == mental_model$limit_df), "Picked wrong limit df")
  })
})
