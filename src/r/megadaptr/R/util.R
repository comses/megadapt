prepend_class <- function(value, cls) {
  classes = c(cls, class(value))
  structure(
    value,
    class = classes
  )
}

call_fnss <- function(fnss, ...) {
  #' Evaluate a function specified system
  #'
  #' @export
  #' @param fnss a function specified system
  #' @param ... the inputs
  UseMethod('call_fnss', fnss)
}

output_dtss <- function(dtss, ...) {
  #' Call the output function of a discrete time specified system
  #'
  #' @export
  #' @param dtss a discrete time specified system
  #' @param ... the arguments
  UseMethod('output_dtss', dtss)
}

transition_dtss <- function(dtss, ...) {
  #' Call the transition function of a discrete time specified system
  #'
  #' @export
  #' @param dtss a discrete time specified system
  #' @param ... the inputs
  UseMethod('transition_dtss', dtss)
}

value_function <- function(model, ...) {
  UseMethod('value_function', model)
}
