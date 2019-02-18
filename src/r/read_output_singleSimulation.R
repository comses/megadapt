R1800 <- tapply(Final_Outcome$Interventions_Ab, list(Final_Outcome$municipio), mean)
write.csv(as.data.frame(R1800), file = "output_interventions_B1800.csv")
