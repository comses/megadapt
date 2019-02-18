# setup file for the MEGADAPT model R -version
# load packages
require(glmmADMB)
# require(maptools)
require(ecr)
require(pscl)
# require(ggplot2)
require(plyr)
# require(ggmap)
# require(ahp) #Analitical hiererquical process package
# require(ahpsurvey)
require(gramEvol) # genetic algorithm optimization
# require(gridExtra)
# require(DT)

# path to data
path_td <- "../../data/"
# path_td<-"home/abaeza/SHV/data/" #path patun
## read shape files
studyArea_CVG_C <- readShapeSpatial(paste(path_td, "ageb_abm_full", sep = ""))
studyArea_CVG_B <- readShapeSpatial(paste(path_td, "agebs_abm_old", sep = ""))
studyArea_CVG <- readShapeSpatial(paste(path_td, "agebs_abm.shp", sep = "")) # for flooding model
clave_municipalities <- data.frame(read.csv(paste(path_td, "claves_estados_delegaciones_CVEGEO.csv", sep = ""), header = T))
#############################################################################
# subset are for CDMX
studyArea_CVG@data$municipio <- as.factor(substring(studyArea_CVG@data$cvgeo, 3, 5))
studyArea_CVG_B@data$municipio <- as.factor(substring(studyArea_CVG_B@data$cvgeo, 3, 5))
studyArea_CVG_C@data$municipio <- as.factor(substring(studyArea_CVG_C@data$CVEGEO, 3, 5))


# new columns with information about municipality and state of the census blocks
# state
studyArea_CVG@data$estado <- as.factor(substring(studyArea_CVG@data$cvgeo, 1, 2))
studyArea_CVG_B@data$estado <- as.factor(substring(studyArea_CVG_B@data$cvgeo, 1, 2))
studyArea_CVG_C@data$estado <- as.factor(substring(studyArea_CVG_C@data$CVEGEO, 1, 2))

# Simulation runs only for the city (CDMX) estado=="09"
studyArea_CVG <- studyArea_CVG[studyArea_CVG$estado == "09", ]
studyArea_CVG_C <- studyArea_CVG_C[studyArea_CVG_C$estado == "09", ]
studyArea_CVG_B <- studyArea_CVG_B[studyArea_CVG_B$estado == "09", ]
source("new_variables_setup.R")
# join A B and C data sets!!!
# PR_2008, rainfall from B

######################################################################################################################
## read data bases
load(paste(path_td, "data_fugas", sep = "")) # data scarcity model
studyArea_CVG@data <- join(studyArea_CVG@data, data.frame(dat_fugas), by = "AGEB_ID") # add fugas to the database
contigency_matrix <- as.matrix(data.frame(read.csv(paste(path_td, "W_matrix_low.csv", sep = ""), header = T)))
######################################################################################################################
# Initiate biophsiical models
# floding
source("r/ponding_model.R")
# scarcity
source("r/water_scarcity_model.R")
# health

######################################################################################################################
## define decision-makers agents
# read file with value functions
source("r/value_functions.R")
# read value function from workshop with sacmex
source("r/value_functions_empirical_parameters.R")
# read mental models as limit and weighted matrices outputs from SUPERDECITION
source("r/read_mental_models.R")
# create MCDA from pairwise comprasisons and create table
source("r/modelo_multicriterio.R")
# initiate site suitability
source("r/site_suitability.R")
# read function to save time-series
source("r/save_results.R")

# load google maps
# xl=c(-98.9, -99.3)
# yl=c(19, 19.6)
# ll<-c(-98.9,19,-99.3,19.6)#
# add map of  mexico city

# m <- leaflet(data=studyArea_CVG) %>%
#  addTiles() %>%  # Add default OpenStreetMap map tiles
#  addMarkers(lng=-99.1398, lat=19.4249,popup="Mexico City") %>%
#   addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%ne
#  setView(lng=mean(xl), lat=mean(yl), zoom = 10)
# m  # Print the map

# map<-get_map(location = c(mean(xl),mean(yl)) ,zoom = 1) #
# map<-get_map(location="Mexico City")
# map<-get_map(location = ll , maptype = "hybrid",source='google',zoom = 10)

# setup parameters
# effectivity_mantenimiento=args[1]
# effectivity_newInfra=args[2]

# effectivity_mantenimiento<-0.01
# effectivity_newInfra<-0.01
Budget <- 750
