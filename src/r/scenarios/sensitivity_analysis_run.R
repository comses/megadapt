library(megadaptr)
source('../scenarios/util.R')


################################################################################################
################################################################################################

#(Originally in file VBSAConvergence.R)
#Convergence of Si and STi indices

# Change these parameters
simyears=1
exp.min=1
exp.max=1
realmodel=T

exponents<-c(exp.min:exp.max)
Ns<-2^exponents
Nmax<-2^exp.max
k<-4


stt<-system.time(results<-VBSA(exp.min,exp.max,simyears,k,realmodel))
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
