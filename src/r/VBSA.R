

VBSA<-function(N,simyears=5) {
  
  #Set global variable of simulation time
  time_simulation<<-simyears
  
  #Names of matrices change according to N
  ABname<-paste("ABMats",as.character(N),".rds",sep="")
  
  #Check whether the matrices were already created
  if (!file.exists(ABname)) {
    source("VBSAMatrices.R")
    createLinearMatrices(N)
  }

  #Assign files to A and B matrices and ABi array
  ABMats<-readRDS(paste("../../VBSAMatrices/",ABname,sep=""))
  
  # Create matrix of results (Y) (columns are: 1=A, 2= B, 3:end=ABi, rows are the results for each parameter combination coming from each matrix)
  #Y<<-matrix(nrow=(2*N),ncol=dim(ABMats)[3]) #get rid of "<<" !!
  
  #Create array of results: two rows for Ab and D, columns correspond to rows in ABMats (1<j<N), and 3rd dim corresponds to the matrices (1<i<k), where 1=A, 2=B, 3:end=ABi
  #It's kind of awkward, but it was the only way I could find to organize the data, given that normVul has two outputs
  Y<-array(dim=c(2,N,(dim(ABMats)[3])))
  
  #Run simulations
  source("VBSArunModel.R")

  Y<-apply(ABMats,c(1,3),normVul) 
  
  #Ab
  Ab.fhat<-(sum( Y[1,,1]+Y[1,,2] )) / (2*N)
  Ab.Vhat<-(sum( (Y[1,,1]-Ab.fhat)^2 + (Y[1,,2]-Ab.fhat)^2 )) / (2*N-1)
  
  Ab.Vi<-(sum( Y[1,,2]*(Y[1,,3:k]-Y[1,,1]) ))/(N)
  Ab.VTi<-(sum( (Y[1,,1]-Y[1,,3:k])^2 ))/(2*N)
  
  Ab.Si<-Ab.Vi/Ab.Vhat
  Ab.STi<-Ab.VTi/Ab.Vhat
  
  #D
  D.fhat<-(sum( Y[2,,1]+Y[2,,2] )) / (2*N)
  D.Vhat<-(sum( (Y[2,,1]-Ab.fhat)^2 + (Y[2,,2]-Ab.fhat)^2 )) / (2*N-1)
  
  D.Vi<-(sum( Y[2,,2]*(Y[2,,3:k]-Y[2,,1]) )) / (N)
  D.VTi<-(sum( (Y[2,,1]-Y[2,,3:k])^2 )) / (2*N)
  
  D.Si<-D.Vi/D.Vhat
  D.STi<-D.VTi/D.Vhat
  
  return(list=c(Ab.Si,Ab.STi,D.Si,D.STi))
}


normVul<-function(x){

  #Run model and get results from last year
  Vlast<-run(x)
  # Vlast<-matrix(c(x,x/2),ncol=2) #toy function to check everything else makes sense
  means<-apply(Vlast,2,mean)

  return(means)
}



