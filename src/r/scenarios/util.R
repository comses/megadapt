library(fs)

data_dir <- function(...) {
  system.file('rawdata', ..., package='megadaptr')
}

output_dir <- function(...) {
  fs::path('../../../../../output/', ...)
}

data_root_dir <- data_dir()

mental_model_file_names = list(
  potable_water_operator_limit = 'potable_water_sacmex_limit.csv',
  non_potable_water_operator_limit = 'sewer_water_sacmex_limit.csv',
  overall_limit = 'resident_limit.csv'
)
