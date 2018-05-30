#' Variables
#'
#' A variable represents values that are associated in the same format. It
#' therefore consists of a list of data frames where one variable represents
#' the values of indicators from the same source.
#'
#' @export

variable <- function(...) {
  ret <- list(...)
  if(length(unique(names(ret))) != length(ret))
    stop("Sources need unique names.")
  for(nam in names(ret)) {
    class(ret[[nam]]) <- c(nam, "variable_el", class(ret[[nam]]))
  }
}

#' @rdname variable
#'
#' @export

as_variable <- function(object, id) {
  UseMethod("as_variable")
}

#' @export

as_variable.data.frame <- function(object, id) {
  if(is.data.frame(id)) {
    if(ncol(object) == 0) object <- as_tibble(id)
    for(nam in colnames(id)) {
      object[[nam]] <-
        structure(id[[nam]], class = c("id", class(id[[nam]])))
    }
  }
  else if(!is.null(id)) {
    if(ncol(object) == 0) object <-
        tibble(id = structure(id, class = c("id", class(id))))
    else object$id <- structure(id, class = c("id", class(id)))
  }
  else {
    if(ncol(object) > 0) stop("Non-trivial variable needs an id column.")
  }
  for(nam in colnames(object)) {
    class(object[[nam]]) <- c(class(object[[nam]]), "variable_col")
    if(nam != "id") class(object[[nam]]) <- c(nam, class(object[[nam]]))
  }
  class(object) <- c("variable", class(object))
  object
}

#' IDs
#'
#' ID columns help identify data and observation units.
#'
#' @param variable
#'
#' @export

ids <- function(variable) {
  ret <- character(0)
  for(col in colnames(variable)) {
    if(is_id(variable[[col]])) ret <- c(ret, col)
  }
  ret
}

#' @rdname ids
#'
#' @export

is_id <- function(x) {
  inherits(x, "id")
}
