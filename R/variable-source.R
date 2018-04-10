#' Source Variables
#'
#' Source Variables do not depend on other variables. They basically initialize
#' the data frame by reading in the external source or generating data in
#' another way that is independent from other variables.
#'
#' @details When applicable, it is recommended to use the file parameter, as it
#' makes the variable's structure clearer
#'
#' @param source_fun the function that reads in the source. May or may not be
#' dependent on the file parameter.
#'
#' @param file optional file that is the source. See details
#'
#' @param ... variable attributes
#'
#' @value An object of class variable

variable_source <- function(source_fun, file = NULL, ...) {
  assert_that(is_function(source_fun))
  ret <- list(
    generate = source_fun,
    file = file
  )
  class(ret) <- c("variable_source", "variable", "list")
  set_properties(ret, ...)
}
