prepend_class <- function(value, cls) {
  classes = c(cls, class(value))
  structure(
    value,
    class = classes
  )
}

call_fnss <- function(fnss, ...) {
  UseMethod('call_fnss', fnss)
}

output_dtss <- function(dtss, ...) {
  UseMethod('output_dtss', dtss)
}

transition_dtss <- function(dtss, ...) {
  UseMethod('transition_dtss', dtss)
}
