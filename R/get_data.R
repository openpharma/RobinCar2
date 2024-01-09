get_data <- function(x, vars) {
  UseMethod("get_data")
}

get_data.data.frame <- function(x, vars) {
  x[, vars]
}

get_data.glm <- function(x, vars) {
  get_data(x$data, vars)
}

get_data.lm <- function(x, vars) {
  if (is.null(x$call$data)) {
    get_data(attr(x$data, ".Environment"), vars)
  } else {
    get_data(eval(x$call$data, envir = attr(x$data, ".Environment")), vars)
  }
}

get_data.environment <- function(x, vars) {
  objs <- mget(vars, envir = x, inherits = TRUE)
  as.data.frame(objs)
}
