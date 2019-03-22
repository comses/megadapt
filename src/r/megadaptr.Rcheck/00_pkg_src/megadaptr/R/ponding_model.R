load_obj <- function(path) {
  env <- new.env()
  nm <- load(path, envir = env)[1]
  env[[nm]]
}

load_ponding_models <- function(base_path) {
  models <- list()
  for (i in 1:9) {
    path <- paste0(base_path, "/encharcamientos/mod_en_reg", i, ".rda")
    models[[i]] <- load_obj(path)
  }
  models
}

#' Generate ponding data
#'
#' @param ponding_models A list of ponding models for each region
#' @return ponding
#' @importFrom gbm predict.gbm
update_ponding <- function(study_data, ponding_models) {
  study_data %>%
    dplyr::group_by(region) %>%
    dplyr::group_map(~ {
      .x %>%
        dplyr::mutate(encharca = predict(ponding_models[[.y$region]],
                                         newdata = .x,
                                         n.trees = 9566,
                                         type = "response"))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(ageb_id, encharca)
}
