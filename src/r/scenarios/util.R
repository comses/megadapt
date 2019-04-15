library(fs)

data_dir <- function(...) {
  fs::path('../../../data/', ...)
}

output_dir <- function(...) {
  fs::path('../../../output/', ...)
}

data_root_dir <- data_dir()

mental_model_file_names = list(
  potable_water_operator_limit = 'DF101215_GOV_AP modificado PNAS.limit.csv',
  non_potable_water_operator_limit = 'SACMEX_Drenaje_limit_SESMO.csv',
  overall_limit = 'I080316_OTR.limit.csv'
)
