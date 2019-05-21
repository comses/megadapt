expect_between <- function(object, lb, ub, allow_empty = FALSE) {
  act <- quasi_label(rlang::enquo(object))

  if (allow_empty) {
    act$lb <- suppressWarnings(min(object))
    act$ub <- suppressWarnings(min(object))
  } else {
    act$lb <- min(object)
    act$ub <- max(object)
  }
  expect(
    act$lb >= lb || act$ub <= ub,
    sprintf('%s has range [%f, %f] not contained in [%f, %f]', act$lab, act$lb, act$ub, lb, ub)
  )

  invisible(act$val)
}
