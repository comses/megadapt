
# (Originally in file VBSAMatrices.R)
# This file creates the matrices used in the Variance-Based Sensitivity Analysis of MEGADAPT
# For this version, we are going to test the effect of 4 parameters, whose particular values are taken from a linear transformation of the Sobol' sequence.

createLinearMatrices <- function(N,k=4) {
  require(qrng)

  # effectivity_newInfra, initially set at 0.07
  par1_min=0.01
  par1_max=0.3

  #effectivity_mantenimiento, initially set at 0.07
  par2_min=0.1
  par2_max=0.3

  #decay_infra, initially set at 0.01
  par3_min=0.001
  par3_max=0.1

  #Budget, initially set at 1800
  par4_min=100
  par4_max=3000

  interc=rep(0,k)
  interc[1]=par1_min
  interc[2]=par2_min
  interc[3]=par3_min
  interc[4]=par4_min
  slope=rep(0,k)
  slope[1]=par1_max - par1_min
  slope[2]=par2_max - par2_min
  slope[3]=par3_max - par3_min
  slope[4]=par4_max - par4_min

  S=sobol(N+4,2*k,skip=1)

  for (i in 1:(2*k)) {
    if (i<=k) {
      S[,i]=S[,i]*slope[i]+interc[i]
    } else {
      S[,i]=S[,i]*slope[i-k]+interc[i-k]
    }
  }

  ABMats=array(dim=c(N,k,(k+2)))
  ABMats[,,]=S[1:N,1:k]
  ABMats[,,2]=S[5:(N+4),(k+1):(2*k)] #Pairing A with B taken from 4 rows below (p.38 from book chapter)
  for (j in 1:k) {
    ABMats[,j,(j+2)]=S[5:(N+4),j+k]
  }

  return(ABMats)

  # ABpath<-paste("/VBSAMatrices/ABMats",as.character(N),".rds",sep="")

  # saveRDS(ABMats,ABpath)

}

################################################################################################
################################################################################################

#(Originally in file VBSAmain.R)
#Running the model and retrieving the variables of interest

run<-function(x,years) {

  set.seed(1000)
  megadapt<-example(x,years)
  # print(megadapt$params)
  results <- simulate_megadapt(megadapt)

  lastT<-max(results$time_sim)
  Vlast<-subset(results,time_sim==lastT,select=c(vulnerability_Ab,vulnerability_D))
  return(Vlast)
}


################################################################################################
################################################################################################

#(Originally in file VBSA.R)
#Sensitivity Analysis. Produces two indexes (first-order and total-order) per variable studied.

VBSA<-function(exp.min,exp.max,simyears=5,k=4,forreal) {

  maxN<-2^exp.max

  #Create matrix
  ABMats<-createLinearMatrices(maxN,k)

  # #Names of matrices change according to N
  # ABname<-paste("ABMats",as.character(maxN),".rds",sep="")
  # #Assign files to A and B matrices and ABi array
  # ABMats<-readRDS(paste("VBSAMatrices/",ABname,sep=""))


  #Create array of results: number of rows corresponds to number of model metrics, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
  #It's kind of awkward, but it was the only way I could find to organize the data, given that modelMetrics can have more than one output
  Y<-array(dim=c(2,maxN,(dim(ABMats)[3])))

  #Run simulations

  Y<-apply(ABMats,c(1,3),function(x) modelMetrics(x,forreal))

  #Calculate sensitivity indices
  #rows<->parameters, columns<->N, third<->(output metrics)*2: first Si for all metrics, then STi for all metrics
  resultss<-array(dim=c(k,(exp.max-exp.min+1),4))

  for (i in exp.min:exp.max) {
    N<-2^i

    Sis<-apply(Y,1,function(x) calc.Si(x,N,k))
    Sis
    STis<-apply(Y,1,function(x) calc.STi(x,N,k))

    resultss[1:k,(i-exp.min+1),]<-c(Sis,STis)
  }

  return(resultss)
}

# Function that runs the model (if "for real")
modelMetrics<-function(x,forreal){

  #Run model and get results from last year
  if (forreal) {
    # source("r/VBSAmain.R")
    Vlast<-run(x,simyears)
  }
  else {
    Vlast<-matrix(c(x,x/2),ncol=2) #toy function to check everything else makes sense
  }

  means<<-apply(Vlast,2,function(x) mean(x,na.rm=T))

  return(means)
}



# Functions to calculate sensitivities

calc.Si<-function(y,N,k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat<<-calc.Vhat(y,N)
  Ares<-y[1:N,1]
  Bres<-y[1:N,2]
  vis<-apply(y[1:N,3:(k+2)],2,function(x) calc.Vi(x,N,Ares,Bres))
  print(vis)
  si<-vis/vhat
  si # returns a matrix where rows are the k parameters, columns are the metrics used as model output
}

calc.Vi<-function(y,N,Ares,Bres) {
  vi<-(sum( Bres * (y - Ares) )) / (N)
  print(vi)
  vi
}


calc.STi<-function(y,N,k) {
  vhat<-calc.Vhat(y,N)
  Ares<-y[1:N,1]
  vti<-apply(y[1:N,3:(k+2)],2,function(x) calc.VTi(x,N,Ares))
  sti<-vti/vhat
  sti
}

calc.VTi<-function(y,N,Ares) {
  vti<-(sum( (Ares - y) ^2 ))/(2*N)
  vti
}


calc.Vhat<-function(y,N) {
  fhat<-(sum( y[1:N,1]+y[1:N,2] )) / (2*N)
  vhat<-(sum( (y[1:N,1]-fhat)^2 + (y[1:N,2]-fhat)^2 )) / (2*N-1)
  vhat
}

################################################################################################
################################################################################################

#(Originally in file VBSAConvergence.R)
#Convergence of Si and STi indices

# Change these parameters
simyears=1
exp.min=1
exp.max=1
forreal=T

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
