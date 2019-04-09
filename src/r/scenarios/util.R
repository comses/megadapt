library(fs)

data_dir <- function(...) {
  fs::path(here::here('../../../data/', ...))
}

output_dir <- function(...) {
  fs::path(here::here('../../../output/', ...))
}
