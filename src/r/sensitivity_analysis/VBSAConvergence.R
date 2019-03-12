# This file helps determine the convergence of Si and STi. These outputs will be evaluated for N values from 2^3 to 2^10

#Define paths
path_to_source<-"./" 
path_to_output<-"../outputs/" 
path_td<-"../data/"
path_to_model<-path_td

source("r/VBSA.R")

# Change these parameters
simyears=1
exp.min=1
exp.max=1
forreal=F

exponents<-c(exp.min:exp.max)
Ns<-2^exponents
Nmax<-2^exp.max
k<-4


stt<-system.time(results<-VBSA(exp.min,exp.max,simyears,k,forreal))
results
  

#Plot both Si and STi vs N
par(mfrow=c(2,2))
colorss<-c("skyblue","orange","magenta")
for (i in 1:dim(results)[3]){
  plot(Ns,results[1,,i],ylim=range(results[,,i]))
  for (j in 2:dim(results)[1]){
    points(Ns,results[j,,i],col=colorss[j-1])
  }
}

