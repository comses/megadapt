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

# from regression tree
## Se hace la prediccion para cada zona (1:9)
update_ponding <- function(study_data, ponding_models) {
  for (hh in 1:9) {
    study_data$encharca[which(study_data$region == hh)] <-
      predict(
        ponding_models[[hh]],
        subset(
          study_data,
          subset = region == hh,
          select = c(
            "f_prec_v",
            "f_esc",
            "n_tramos",
            "q100",
            "bombeo_tot",
            "rejillas"
          )
        ),
        # observaciones de todas las variables de la region
        n.trees = 9566,
        # Número de árboles que usa el modelo
        type = "response"
      )
  }
  print(study_data$encharca[1708:1710])
  
  study_data
}