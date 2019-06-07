path_name_ <- function(path, name) {
  ifelse(path == "", name, paste0(path, '$', name))
}

check_shape <- function(x, shape, path = '') {
  missing_fields <- checkmate::check_names(names(x), permutation.of = names(shape))
  if (is.character(missing_fields)) {
    return(ifelse(path == '', missing_fields, stringr::str_glue('Path {path}: {missing_fields}')))
  }
  non_shape_fields <- purrr::discard(names(shape), function(name) is.list(shape[[name]]))
  field_check_failures <- purrr::map_chr(non_shape_fields, function(name) {
    check_result <- shape[[name]](x[[name]])
    ifelse(is.character(check_result), paste0('Field ', path_name_(path, name), ': ', check_result), '')
  }) %>% purrr::discard(function(e) e == '')
  if (length(field_check_failures) > 0) {
    return(paste0(field_check_failures, collapse = '\n'))
  }

  shape_fields <- setdiff(names(shape), non_shape_fields)

  list_type_checks <- purrr::map_chr(shape_fields, function(name) {
    check_result <- checkmate::check_list(x[[name]])
    ifelse(is.character(check_result), paste0('Field ', path_name_(path, name), ': ', check_result), '')
  }) %>% purrr::discard(function(e) e == '')
  if (length(list_type_checks) > 0) {
    return(paste0(list_type_checks, collapse = '\n'))
  }

  nested_shape_checks <- purrr::map(
    shape_fields,
    function(name) check_shape(x = x[[name]],
                               shape = shape[[name]],
                               path = path_name_(path, name))) %>%
    purrr::discard(is.logical)
  if (length(nested_shape_checks) > 0) {
    return(paste0(nested_shape_checks, collapse = '\n'))
  }

  TRUE
}

assert_shape <- checkmate::makeAssertionFunction(check_shape)

flatten_list_recursive <- function(x) {
  kvs <- list()
  flattener_ <- function(x, path = '') {
    if (is.list(x)) {
      ns <- names(x)
      for (name in ns) {
        flattener_(x[[name]], path = ifelse(path == '', name, paste0(path, '__', name)))
      }
    } else {
      kvs[[path]] <<- x
    }
  }
  flattener_(x)
  kvs
}

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
