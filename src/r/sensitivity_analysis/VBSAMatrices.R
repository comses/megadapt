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
  
  ABpath<-paste("../VBSAMatrices/ABMats",as.character(N),".rds",sep="")
  
  saveRDS(ABMats,ABpath)

}

