#setup file for the MEGADAPT model R -version
#load packages
require(glmmADMB)
require(maptools)
require(ecr)
require(pscl)
require(gbm)
#require(ggplot2)
require(plyr) #Tools for Splitting, Applying and Combining Data
#require(ggmap)
#require(ahp) #Analitical hiererquical process package
#require(ahpsurvey)
require(gramEvol) #genetic algorithm optimization
#require(gridExtra)
#require(DT)

#path to data
path_td<-"../../data/"
path_to_model<-path_td
#path_td<-"../data/"
#path_td<-"home/abaeza/SHV/data/" #path patun
##read shape files
studyArea_CVG<-readShapeSpatial(paste(path_td,'Layer_MEGADAPT_Oct2018.shp',sep=""))#for flooding model
#############################################################################
#subset are for CDMX
#Simulation runs only for the city (CDMX) estado=="09"
studyArea_CVG <- studyArea_CVG[studyArea_CVG$estado== "09",]
source('new_variables_setup.R')
######################################################################################################################
##read data contigency matrix
contigency_matrix<-as.matrix(data.frame(read.csv(paste(path_td,"W_matrix_low.csv",sep=""),header = T)))
######################################################################################################################
#Initiate biophsiical models
 #floding
source("ponding_model.R")
 #scarcity
source("water_scarcity_model.R")
 #health

######################################################################################################################
#read scenarios of climate change
#read scenarios of rainfall and run-offs for emmisions 8.5.
S_85=read.csv(paste(path_td,"df_prec_esc_85_85.csv",sep=""))
######################################################################################################################
##define decision-makers agents
#read file with value functions
source("value_functions.R")
#read value function from workshop with sacmex
source("value_functions_empirical_parameters.R")
#read mental models as limit and weighted matrices outputs from SUPERDECITION
source("read_mental_models.R")
#create MCDA from pairwise comprasisons and create table
#initiate site suitability
source("site_suitability.R")
#read function to save time-series
source("save_results.R")
#Adaptation: set parameters of sensitivity such that
#10 years will take to a neighborhood to be  half sensitivity to hazards
hsc_Ab<-10
hsc_D<-10

#Create series of times in weeks from 2019 until the years of simulations
ini_date=seq.Date(from =as.Date("2019/1/1"), to =as.Date(sprintf("20%s/1/1",(19+time_simulation))),by="week") 
year_ts=format(as.Date(ini_date), "%Y")
month_ts=format(as.Date(ini_date), "%m")

#Generate a bollean series, with value 1 when month or year change and 0 otherwise
month_change=c(0,diff(as.numeric(month_ts)))
month_change[which(month_change==(-11))]=1 #this change the values when the difference whas from december (12) to january (1) (-11) 
year_change=c(1,diff(as.numeric(year_ts)))

