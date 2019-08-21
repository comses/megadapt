# define value functions
#######################################################################################################
logistic_vf <- function(x, k, center, xmax, xmin) {
  x <- max(c(x, xmin))
  x <- min(c(x, xmax))
  return(1 / (1.0 + exp(-k * ((100 * (x - xmin) / (xmax - xmin)) - (100 * (center - xmin) / (xmax - xmin))))))
}
#######################################################################################################
logistica_invertida <- function(x, k, center, xmax, xmin) {
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
  x <- pmax(x, xmin)
  x <- pmin(x, xmax)
  return(1.0 - gaussian(x, a, center, xmax, xmin))
}
#######################################################################################################
concava_decreciente <- function(x, gama, xmax, xmin) {
  x <- pmax(x, xmin)
  x <- pmin(x, xmax)
  return(((exp(gama * (100.0 - (100.0 * (x - xmin) / (xmax - xmin))))) - 1) / (exp(gama * 100) - 1))
}
#######################################################################################################
concava_creciente <- function(x, gama, xmax, xmin) {
  x <- pmax(x, xmin)
  x <- pmin(x, xmax)
  return(((exp(gama * (100 * (x - xmin) / (xmax - xmin)))) - 1) / (exp(gama * 100) - 1))
}
#######################################################################################################
convexa_decreciente <- function(x, gama, xmax, xmin) {
  x <- pmax(x, xmin)
  x <- pmin(x, xmax)
  return(1.0 - concava_creciente(x, gama, xmax, xmin))
}
#######################################################################################################
convexa_creciente <- function(x, gama, xmax, xmin) {
  x <- pmax(x, xmin)
  x <- pmin(x, xmax)
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

lack_of_infrastructure_vf <- function(x, saturation=1,x_max) { # p1 scale parameter
  p1 <- (-log10(log10(1.1 + 0.88 * (10 - saturation)))) / ((log10(x_max))^2)

  y_vf <- exp(0 * p1)
  yy_vf <- exp(100 * p1)
  svf <- 1 - ((exp(x * p1) - y_vf) / (yy_vf - y_vf))
  return(ifelse(test = svf > 0, yes = svf, 0))
}

#######################################################################################################
health_vf <- function(x, max_x,saturation) { #
control_parameter=10^(3/10*(log(max_x^5)-saturation))
  yy_vf <- 1 - exp((0 - 10) / control_parameter)
  y_vf <- 1 - exp((max_x - 10) / control_parameter)
  svf <- ((1 - exp((x - 10) / control_parameter)) - y_vf) / (yy_vf - y_vf)
  return(ifelse(test = svf > 0, yes = svf, no = 0))
}
#######################################################################################################
falta_infrastructure_resident_vf <- function(x, p1 = 0.3457691) { # p1=0.3457691
  y_vf <- exp(p1 * 0)
  yy_vf <- exp(p1 * 100)
  return((exp(p1 * (x)) - y_vf) / (yy_vf - y_vf))
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
Peticion_Delegaciones_vf <- function(x) {
  return(1 - x)
}
#######################################################################################################
urban_growth_f <- function(x, xmax) {
  return(x / xmax)
}


############################################################## 3
################################################################
#' A value function with cutoffs following the Weber–Fechner progression
#' @param x a number or a vector of values of a variables to be changed to a [1-0] scale
#' @param xcuts A set of cutoff proportions to divide variables to be changed. The default option is the Weber–Fechner progression
#' @param xmax  The maximum value fo the variable to be changed
#' @param ycuts The new values obtained under under the new [0,1] scale
#' @return A number or a vector of values tranformed to a scale [0,1]

Value_Function_cut_offs <- function(x, xcuts = c(0.0625, 0.125, 0.25, 0.5), xmax, ycuts = c(0.2, 0.4, 0.6, 0.8, 1)) {
  if (x > xcuts[4] * xmax) SM <- ycuts[5]
  if (x > xcuts[3] * xmax & x <= xcuts[4] * xmax) SM <- ycuts[4]
  if (x > xcuts[2] * xmax & x <= xcuts[3] * xmax) SM <- ycuts[3]
  if (x > xcuts[1] * xmax & x <= xcuts[2] * xmax) SM <- ycuts[2]
  if (x <= xcuts[1] * xmax) SM <- ycuts[1]

  return(SM)
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

#####################################################################
#' Calculate the distance to an ideal point using a "compromized programing" metric
#'@param criteria_weights a vector of weights from the criteria part in a limit vector
#'@param X_vf a matrix of value functions outcomes associated to the criteria part of the limit vector
#'@param alternative_weights a vector of alternative weight associated to the alternatives part of the limit vector
#'@param exponent A value to define the type of distance exponent=2: Euclidean, exponent=1: Manhattan
#'@return A vector of distances to the ideal point.
ideal_distance <- function(X_vf, criteria_weights, alternative_weights, exponent = 1) {
  return(((alternative_weights ^ exponent) * rowSums((criteria_weights^exponent) * ((1 - X_vf)^exponent), na.rm = T))^(1 / exponent))
}



