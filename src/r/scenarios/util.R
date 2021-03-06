library(fs)

data_dir <- function(...) {
  system.file('rawdata', ..., package='megadaptr')
}

output_dir <- function(...) {
  fs::path('../../../output/', ...)
}

data_root_dir <- data_dir()

mental_model_strategies <- megadaptr:::mental_model_constant_strategies()
