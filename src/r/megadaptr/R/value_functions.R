# define value functions
#######################################################################################################
logistic_vf <- function(x, k, center, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1 / (1.0 + exp(-k * ((100 * (x - xmin) / (xmax - xmin)) - (100 * (center - xmin) / (xmax - xmin))))))
}
#######################################################################################################
logistic_invertida <- function(x, k, center, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1.0 - logistic_vf(x, k, center, xmax, xmin))
}
#######################################################################################################
gaussian <- function(x, a, center, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(exp(0.0 - ((((100 * (x - xmin) / (xmax - xmin)) - (100 * (center - xmin) / (xmax - xmin))) / (a))^2)))
}
#######################################################################################################
campana_invertida <- function(x, a, center, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1.0 - gaussian(x, a, center, xmax, xmin))
}
#######################################################################################################
concava_decreciente <- function(x, gama, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(((exp(gama * (100.0 - (100.0 * (x - xmin) / (xmax - xmin))))) - 1) / (exp(gama * 100) - 1))
}
#######################################################################################################
concava_creciente <- function(x, gama, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(((exp(gama * (100 * (x - xmin) / (xmax - xmin)))) - 1) / (exp(gama * 100) - 1))
}
#######################################################################################################
convexa_decreciente <- function(x, gama, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1.0 - concava_creciente(x, gama, xmax, xmin))
}
#######################################################################################################
convexa_creciente <- function(x, gama, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1.0 - concava_decreciente(x, gama, xmax, xmin))
}
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
# value functions from expert knowdledge

drainages_clogged_vf <- function(x, Valor_minimo_Y_en_X, amplitude = 5000) { # ;value function criterium: garbage or "drainages-clogged"
  #browser()
  y_vf <- (exp(-((0 - Valor_minimo_Y_en_X) / amplitude)^2))
  yy_vf <- (exp(-((30 - Valor_minimo_Y_en_X) / amplitude)^2))

  svf <- 1 - (((exp(-((x - Valor_minimo_Y_en_X) / amplitude)^2)) - y_vf) / (yy_vf - y_vf))
  if (svf > 0) {
    return(svf)
  }
  else {
    return(0)
  }
}

#######################################################################################################
scarcity_residents_vf <- function(x, p1 = 0.115526234) { #
  y_vf <- exp(-(28 * p1))
  yy_vf <- exp(-(0 * p1))
  svf <- (exp(-(x * p1)) - y_vf) / (yy_vf - y_vf)
  return(ifelse(test = svf > 0, yes = svf, 0))
}
#######################################################################################################
scarcity_residents_empirical_vf <- function(x, tau = 12) { #
  x >= tau
}

#######################################################################################################

lack_of_infrastructure_vf <- function(x, saturation=1,x_max) { # p1 scale parameter
  p1 <- (-log10(log10(1.1 + 0.88 * (10 - saturation)))) / ((log10(x_max))^2)

  y_vf <- exp(0 * p1)
  yy_vf <- exp(100 * p1)
  svf <- 1 - ((exp(x * p1) - y_vf) / (yy_vf - y_vf))
  return(ifelse(test = svf > 0, yes = svf, 0))
}

#######################################################################################################
water_quality_residents_vf <- function(x, p1 = 0.3457691) { #
  y_vf <- exp(0 * p1)
  yy_vf <- exp(100 * p1)
  svf <- ((exp(x * p1)) - y_vf) / (yy_vf - y_vf)
  return(ifelse(test = svf > 0, yes = svf, 0))
}
#######################################################################################################
health_vf <- function(x, p1 = 15.848931) { #

  yy_vf <- 1 - exp((0 - 10) / p1)
  y_vf <- 1 - exp((100 - 10) / p1)
  svf <- ((1 - exp((x - 10) / p1)) - y_vf) / (yy_vf - y_vf)
  return(ifelse(test = svf > 0, yes = svf, 0))
}
#######################################################################################################
scarcity_residents_empirical_vf <- function(x, tau) { # tau=12 for protesting #tau=6 for adaptation
  return(ifelse(test = x > tau, yes = 0, no = 1))
}
#######################################################################################################
capacity_drainage_vf <- function(x, sat, x_max, x_min) { # ,p1=0.174916383
  if (is.na(x) == FALSE) {
    p1 <- (-log10(log10(1.1 + 0.88 * (10 - sat)))) / ((log10(x_max))^2)
    #        x=x/10000000
    y_vf <- exp(p1 * x_min)
    yy_vf <- exp(p1 * x_max)
    svf <- (exp(p1 * x) - y_vf) / (yy_vf - y_vf)
    if (x > x_max) svf <- 1
    if (x < 0) svf <- 0
    return(svf)
  }
  else {
    return(NA)
  }
}
#######################################################################################################
falta_infrastructure_resident_vf <- function(x, p1 = 0.3457691) { # p1=0.3457691
  y_vf <- exp(p1 * 0)
  yy_vf <- exp(p1 * 100)
  return((exp(p1 * (x)) - y_vf) / (yy_vf - y_vf))
}
#######################################################################################################
scarcity_sacmex_vf <- function(x, p1 = 0.020455577, xmax = 336, xmin = 0) { # p1=0.115526234
  y_vf <- exp(-(xmax * p1))
  yy_vf <- exp(-(xmin * p1))
  svf <- (exp(-(x * p1)) - y_vf) / (yy_vf - y_vf)
  return(ifelse(test = svf > 0, yes = svf, 0))
}
#######################################################################################################
social_pressure_vf <- function(x, p1 = 20, p2 = 46) {
  y_vf <- (exp(-((0 - p2) / p1)^2))
  yy_vf <- (exp(-((54 - p2) / p1)^2))
  return(1 - ((exp(-((x - p2) / p1)^2)) - y_vf) / (yy_vf - y_vf))
}
#######################################################################################################
flooding_vf <- function(x) {
  return(ifelse(test = x > 0, yes = 1, no = 0))
}
#######################################################################################################
ponding_vf <- function(x, p1 = 30, xmax = 100, xmin = 0) {
  y_vf <- exp(-((xmax / p1)^2))
  yy_vf <- exp(-((xmin / p1)^2))
  return((exp(-((x / p1)^2)) - y_vf) / (yy_vf - y_vf))
}
#######################################################################################################
rainfall_vf <- function(x, p1 = 742.8737773, xmax = 1300, xmin = 0) {
  y_vf <- 1 - exp((xmax - 10) / p1)
  yy_vf <- 1 - exp((xmin - 10) / p1)
  svf <- ((1 - exp((x - 10) / p1)) - y_vf) / (yy_vf - y_vf)
  if (x > xmax) svf <- 0
  return(svf)
}
#######################################################################################################
run_off_vf <- function(x, xmax = 50, xmin = 100) {
  y_vf <- exp(-(((0 - xmax) / xmin)^2))
  yy_vf <- exp(-(((xmax - xmax) / xmin)^2))
  svf <- 1 - (exp(-(((x - xmax) / xmin)^2)) - y_vf) / (yy_vf - y_vf)
  if (x > xmax) svf <- 0
  return(svf)
}
#######################################################################################################
pression_medios_vf <- function(x, p1 = 0.005268212, xmin = 0, xmax = 600) {
  y_vf <- (exp(-xmin * p1))
  yy_vf <- (exp(-xmax * p1))
  svf <- 1 - ((exp(-x * p1)) - y_vf) / (yy_vf - y_vf)
  if (x > xmax) svf <- 0
  return(svf)
}
#######################################################################################################
Peticiones_usuarios_vf <- function(x, xmax) {
  return(1 - (x / xmax))
}

#######################################################################################################
Peticion_Delegaciones_vf <- function(x) {
  return(1 - x)
}
#######################################################################################################
urban_growth_f <- function(x, xmax) {
  return(x / xmax)
}


############################################################## 3
################################################################
# value function with cutoffs


Value_Function_cut_offs <- function(x, xcuts = c(0.0625, 0.125, 0.25, 0.5), xmax, ycuts = c(0.2, 0.4, 0.6, 0.8, 1)) {
  if (x > xcuts[4] * xmax) SM <- ycuts[5]
  if (x > xcuts[3] * xmax & x <= xcuts[4] * xmax) SM <- ycuts[4]
  if (x > xcuts[2] * xmax & x <= xcuts[3] * xmax) SM <- ycuts[3]
  if (x > xcuts[1] * xmax & x <= xcuts[2] * xmax) SM <- ycuts[2]
  if (x <= xcuts[1] * xmax) SM <- ycuts[1]

  return(SM)
}

#######################################################################################
# this function calcualte a distance to ideal point using compromized programing metric
# arguments:

# y: a vector of attributes converted to 0-1 scale using value functions
# x a list of weights from the mental model
# z=alaternative weight
# exponent: to control the type of distance h_Cp=2 euclidian# h_Cp=1 manhattan

ideal_distance <- function(x, y, exponent = 1, z) {
  return((z * rowSums((y^exponent) * ((1 - x)^exponent), na.rm = T))^(1 / exponent))
}


load_value_function_config <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE)
  params <- as.list(df %>% tidyr::spread(V1, V2))
  param_names <- names(params)

  numeric_keys <- c('a', 'center', 'gama', 'k', 'min', 'max')
  for (numeric_key in numeric_keys) {
    if (numeric_key %in% param_names) {
      params[[numeric_key]] <- as.numeric(params[[numeric_key]])
    }
  }

  if ('show_map' %in% param_names) {
    params[['show_map']] <- as.logical(params[['show_map']])
  }

  params
}

create_value_function_config <- function(sewer_age,
                                         shortage_age,
                                         salt_water_quality,
                                         shortage_failures,
                                         hours_of_service_failure,
                                         hydraulic_pressure_failure,
                                         subsidence) {
  list(
    sewer_age = sewer_age,
    shortage_age = shortage_age,
    salt_water_quality = salt_water_quality,
    shortage_failures = shortage_failures,
    hours_of_service_failure = hours_of_service_failure,
    hydraulic_pressure_failure = hydraulic_pressure_failure,
    subsidence = subsidence
  )
}
