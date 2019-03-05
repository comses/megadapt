
# this functions elevates the unweighted matrix to the power
# of p.
# The arguments of this functions are
# 1)Unweighted super matrix matrix
# 2) the power to elevate the matrix
# With a sufficiently large value for p, the the outcome of this function
# is a new matrix that should be the limit matrix
mpot <- function(UW_matrix, p) {
  # calculates A^p (matrix multiplied p times with itself)
  # inputes: A - real-valued square matrix, p - natural number.
  # output:  A^p

  UW_matrix_B <- UW_matrix
  if (p > 1) {
    for (i in 2:p)
      UW_matrix_B <- UW_matrix_B %*% UW_matrix
  }
  return(UW_matrix_B)
}



# example
# elevate the matrix
lim_supermatrix <- mpot(weighted_supermatrix, 100)
# obtain the eighenvalues
eigh_v <- lim_supermatrix[, 1]

# normalize the eighenvalues to obtain the final weights
# criteria
Criteria_sacmcx_D <- eigh_v[-c(1, 2)] / sum(eigh_v[-c(1:2)])
# actions
alternative_weights_D <- eigh_v[c(1, 2)] / sum(eigh_v[c(1:2)])
