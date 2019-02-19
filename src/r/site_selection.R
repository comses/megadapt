determine_site_selection <- function(site_suitability, budget) {
  # first find the ranking of non-dominant solutions in the pareto frontier
  r <- doNondominatedSorting(
    rbind(
      site_suitability$distance_ideal_A1_D,
      site_suitability$distance_ideal_A2_D,
      site_suitability$distance_ideal_A1_Ab,
      site_suitability$distance_ideal_A2_Ab
    )
  )$ranks

  # save the solutions up to the budget value (750 now but it will change dinamically)
  ss_A1_D <- site_suitability$distance_ideal_A1_D[order(r)[1:budget]]
  ss_A2_D <- site_suitability$distance_ideal_A2_D[order(r)[1:budget]]
  ss_A1_Ab <- site_suitability$distance_ideal_A1_Ab[order(r)[1:budget]]
  ss_A2_Ab <- site_suitability$distance_ideal_A2_Ab[order(r)[1:budget]]

  choices <- max.col(as.matrix(cbind(ss_A1_D, ss_A2_D, ss_A1_Ab, ss_A2_D)))

  # save ID of selected agebs
  selected_agebs <- order(r)[1:budget]

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
