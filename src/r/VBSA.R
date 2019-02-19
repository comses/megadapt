

VBSA<-function(exp.min,exp.max,simyears=5,k=4,forreal) {
  
  #Set global variable of simulation time
  time_simulation<<-simyears
  
  maxN<-2^exp.max
  
  #Names of matrices change according to N
  ABname<-paste("ABMats",as.character(maxN),".rds",sep="")
  
  #Check whether the matrices were already created
  if (!file.exists(ABname)) {
    source("r/VBSAMatrices.R")
    createLinearMatrices(maxN,k)
  }

  #Assign files to A and B matrices and ABi array
  ABMats<<-readRDS(paste("../VBSAMatrices/",ABname,sep=""))
  
  
  #Create array of results: number of rows corresponds to number of model metrics, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k+2), where 1=A, 2=B, 3:end=ABi
  #It's kind of awkward, but it was the only way I could find to organize the data, given that modelMetrics can have more than one output
  Y<<-array(dim=c(2,maxN,(dim(ABMats)[3])))
  
  #Run simulations
  source("r/VBSArunModel.R")

  Y<<-apply(ABMats,c(1,3),function(x) modelMetrics(x,forreal)) 
  
  results<-array(dim=c(k,(exp.max-exp.min+1),4))
  
  for (i in exp.min:exp.max) {
    N<-2^i
    
    Sis<-apply(Y,1,function(x) calc.Si(x,N,k))
    STis<-apply(Y,1,function(x) calc.STi(x,N,k))
    
    results[1:k,(i-exp.min+1),]<-c(Sis,STis)
  }
  
  return(results)
}

# Function that runs the model (if "for real")
modelMetrics<-function(x,forreal){

  #Run model and get results from last year
  if (forreal) {
    Vlast<-run(x)
  }
  else {
    Vlast<-matrix(c(x,x/2),ncol=2) #toy function to check everything else makes sense
  }
  
  means<-apply(Vlast,2,mean)

  return(means)
}



# Functions to calculate sensitivities

calc.Si<-function(y,N,k) {
  #y is a "slice" from the array Y. Columns (1<j<N) of Y turn into rows in y, and third dim (1<i<k+2) turns into columns.
  vhat<-calc.Vhat(y,N)
  Ares<-y[1:N,1]
  Bres<-y[1:N,2]
  vis<-apply(y[1:N,3:(k+2)],2,function(x) calc.Vi(x,N,Ares,Bres))
  si<-vis/vhat
  si # returns a matrix where rows are the k parameters, columns are the metrics used as model output
}

calc.Vi<-function(y,N,Ares,Bres) {
  vi<-(sum( Bres * (y - Ares) )) / (N)
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
}


calc.Vhat<-function(y,N) {
  fhat<-(sum( y[1:N,1]+y[1:N,2] )) / (2*N)
  vhat<<-(sum( (y[1:N,1]-fhat)^2 + (y[1:N,2]-fhat)^2 )) / (2*N-1)
  vhat
}
