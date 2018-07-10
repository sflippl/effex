#' Keys
#'
#' Keys are defined as columns of a table resp. arguments of a function that
#' uniquely determine specific observations resp. the function's return. They
#' simplify management of a collection as the canonical way of splitting up data
#' frames.
#'
#' @param x an R object
#'
#' @return The keys of x.
#'
#' @export

key <- function(x) {
  UseMethod("key")
}

#' @describeIn key
#'
#' @export

key.data.frame <- function(x) {
  attr(x, "key")
}

#' @describeIn key
#'
#' @export

key.function <- function(x) {
  key <- attr(x, "key")
  if(is.null(key)) return(names(formals(x)))
  else return(key)
}

#' @describeIn key
#'
#' @export

key.default <- function(x) {
  message("Objects of the class ", class(x), " cannot have a key.",
          " Only data.frames and functions have keys.\n")
  NULL
}

#' @describeIn key
#'
#' Does x have a key?
#'
#' @export

has_key <- function(x) !is.null(key(x))


#' @describeIn key
#'
#' @param key character vector. Each element must be a column of the data frame
#' resp. an argument of the function.
#'
#' @return x with new key `key`
#'
#' @export

change_key <- function(x, key) {
  UseMethod("change_key")
}

#' @describeIn key
#'
#' @export

change_key.data.frame <- function(x, key) {
  assertthat::assert_that(all(key %in% colnames(x)))
  assertthat::assert_that(nrow(unique(dplyr::select(x, key))) ==
                            nrow(dplyr::select(x, key)))
  attr(x, "key") <- key
  x
}

#' @describeIn key
#'
#' @export

change_key.function <- function(x, key) {
  assertthat::assert_that(all(key %in% names(formals(x))))
  attr(x, "key") <- key
  x
}

#' @describeIn key
#'
#' @export

change_key.default <- function(x, key) {
  stop("Objects of the class ", class(x), " cannot have a key.",
       " Only data.frames and functions have keys.\n")
}

#' @describeIn key
#'
#' @export

`key<-` <- function(x, value) {
  change_key(x, value)
}
