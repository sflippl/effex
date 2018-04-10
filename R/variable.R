#' Variables
#'
#' Variables are the fundamental unit of information.
#'
#' @param x an object in R.
#'
#' @describeIn is_variable Is x a variable?
#'
#' @export

is_variable <- function(x) {
  ret <- inherits(x, "variable")
  if(ret & !is_variable_source(x) & !is_variable_dependent(x))
    warning("Ill-formed variable: neither a source nor a dependent variable.")
  ret
}

#' @describeIn is_variable Is x a source variable?
#'
#' @export

is_variable_source <- function(x) {
  ret <- inherits(x, "variable_source")
  if(ret & !is_variable(x))
    warning("Ill-formed source variable: is not a variable.")
}

#' @describeIn is_variable Is x a dependent variable?
#'
#' @export

is_variable_dependent <- function(x) {
  ret <- inherits(x, "variable_dependent")
  if(ret & !is_variable(x))
    warning("Ill-formed dependent variable: is not a variable.")
}
