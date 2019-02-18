# statistical model of the relationship between socio-hydrological variables and poonding events
# studyArea_CVG@data$estado<-as.factor(substring(studyArea_CVG@data$cvgeo,1,2))
# IDD<-studyArea_CVG@data$AGEB_ID[which(studyArea_CVG@data$estado=='09')]

# studyArea_CVG@data$AveR<-studyArea_CVG@data$PR_2014
# studyArea_CVG@data$BASURA<-studyArea_CVG@data$BASURA/1000
# studyArea_CVG@data$encharca<-round(studyArea_CVG@data$encharca)
# studyArea_CVG@data$escurri<-studyArea_CVG@data$escurri/1000
# fit_zinbinom <- glmmadmb(encharca~antiguedad_D+subsidenci+PR_2014+escurri+BASURA,data =studyArea_CVG@data,zeroInflation=TRUE, family='nbinom1')
### Sesmo model commented#####
##################################################################################
# load regresion tree models developed by the LANCIS team at UNAM
# https://github.com/sostenibilidad-unam/SHV/issues/83

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
Modelos <- load_ponding_models(path_to_model)
