#site selection

#first find the ranking of non-dominant solutions in the pareto frontier
r<-doNondominatedSorting(rbind(distance_ideal_A1_D,distance_ideal_A2_D,distance_ideal_A1_Ab,distance_ideal_A2_Ab))$ranks

#save the solutions up to the budget value (750 now but it will change dinamically)
ss_A1_D<-distance_ideal_A1_D[order(r)[1:Budget]]
ss_A2_D<-distance_ideal_A2_D[order(r)[1:Budget]]
ss_A1_Ab<-distance_ideal_A1_Ab[order(r)[1:Budget]]
ss_A2_Ab<-distance_ideal_A2_Ab[order(r)[1:Budget]]

#save ID of selected agebs 
#selected_agebs<-studyArea_CVG@data$AGEB_ID[order(r)[1:Budget]]
selected_agebs<-order(r)[1:Budget]

#source Genetic algorith evaluation functions 
source("genetic_Algorithm.R")

#Run optimization
cromosoma_resultante<-run_GA()

#Store ID of agebs that will be modified by sacmex 
A1<-selected_agebs[which(cromosoma_resultante==1)] #"Mantenimiento" D
A2<-selected_agebs[which(cromosoma_resultante==2)] #"Nueva_infraestructura" D
A3<-selected_agebs[which(cromosoma_resultante==3)] #"Mantenimiento" Ab
A4<-selected_agebs[which(cromosoma_resultante==4)] #"Nueva_infraestructura" Ab



