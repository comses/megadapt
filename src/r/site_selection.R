determine_site_selection <- function(site_suitability, budget) {
  # first find the ranking of non-dominant solutions in the pareto frontier
  r <- 
    rbind(
      site_suitability$distance_ideal_A1_D,
      site_suitability$distance_ideal_A2_D,
      site_suitability$distance_ideal_A1_Ab,
      site_suitability$distance_ideal_A2_Ab
    )

  choices <- max.col(as.matrix(r))

  # save ID of selected agebs
  selected_agebs <- order(r)[1:min(budget, length(r))]

  # Store ID of agebs that will be modified by sacmex
  A1 <- selected_agebs[which(choices == 1)] # "Mantenimiento" D
  A2 <- selected_agebs[which(choices == 2)] # "Nueva_infraestructura" D
  A3 <- selected_agebs[which(choices == 3)] # "Mantenimiento" Ab
  A4 <- selected_agebs[which(choices == 4)] # "Nueva_infraestructura" Ab

  list(
    A1 = A1,
    A2 = A2,
    A3 = A3,
    A4 = A4
  )
}
