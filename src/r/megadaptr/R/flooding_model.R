load_obj <- function(path) {
  env <- new.env()
  nm <- load(path, envir = env)[1]
  env[[nm]]
}

load_flooding_models <- function(base_path) {
  models <- list()
  for (i in 1:9) {
    path <- paste0(base_path, "/inundaciones/mod_inund_reg", i, ".rda")
    models[[i]] <- load_obj(path)
  }
  models
}

#' Generate ponding data
#'
#' @param flooding_models A list of model objects to estimate frequency of flooding for each region
#' @return floodings
#' @importFrom gbm predict.gbm
update_flooding <- function(study_data, flooding_models) {
  study_data %>%
    dplyr::group_by(region) %>%
    dplyr::group_map(~ {
      .x %>%
        dplyr::mutate(inunda = predict(flooding_models[[.y$region]],
                                         newdata = .x,
                                         n.trees = 9566,
                                         type = "response"))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(ageb_id, inunda)
}
