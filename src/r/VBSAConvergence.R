# This file helps determine the convergence of Si and STi. These outputs will be evaluated for N values from 2^3 to 2^10

#Define paths
path_to_source<-"./" 
path_to_output<-"../../outputs/" 
path_td<-"../../data/"
path_to_model<-path_td

source("VBSA.R")
simyears=2

exponents<-c(1:2) #change 
Ns<-2^exponents
k=4
results<-matrix(nrow = 4, ncol = (exponents[2]-exponents[1]+1))
system.time(
results<-sapply(Ns,function(x)(VBSA(x,simyears)))  
)
  
  

#Plot both Si and STi vs N