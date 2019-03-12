source('r/setup.R')
source('r/example.R')

require(dplyr)

run<-function(x,years) {
  # #Assign values to variables
  # effectivity_newInfra<<-x[1]
  # effectivity_mantenimiento<<-x[2]
  # decay_infra<<-x[3]
  # Budget<<-x[4]
  
  set.seed(1000)
  megadapt<-example(x,years)
  # print(megadapt$params)
  results <- simulate_megadapt(megadapt)
  
  lastT<-max(results$time_sim)
  Vlast<-subset(results,time_sim==lastT,select=c(vulnerability_Ab,vulnerability_D))
  return(Vlast)
}
