######################################################################################################################
############## Run the MEGADAPT MODEL
######################################################################################################################



path_to_source<-"./" #change path to use it
path_to_output<-"../../outputs/" #change path to use it
setwd(path_to_source)

#Set Parameter Values of the Simulation
source("initial_parameter_values.R")
#Setup the Model
source("setup.R")
#Run the Model
source("cycle.R")