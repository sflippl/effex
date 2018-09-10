#' Metaframe
#'
#' The metaframe is the place in which the effex information is maintained. It
#' consists of columns with exclusively reserved names many of which are used by
#' certain effex functions. These names are documented in the respective
#' functions. Required column names for any metaframe are:
#'
#' * `name`: identify the column name
#'
#' @export

new_metaframe <- function(...) {
  as_metaframe(data.frame(..., stringsAsFactors = FALSE))
}

#' @rdname metaframe
#'
#' @param df a data frame that contains information on indicators
#'
#' @export

as_metaframe <- function(df) {
  validate_metaframe(df)
  if(is_metaframe(df)) return(df)
  structure(df, class = c("metaframe", class(df)))
}

validate_metaframe <- function(df) {
  assertthat::assert_that(is.data.frame(df), "name" %in% colnames(df))
}

#' @rdname metaframe
#'
#' @param x an object
#'
#' @export

is_metaframe <- function(x) {
  inherits(x, "metaframe")
}

#' Metaframe of data
#'
#' Get and set the [metaframe()] of certain data.
#'
#' @param x an object
#'
#' @export

metaframe <- function(x) {
  mf <- attr(x, "metaframe")
  # As the metaframe attribute can be added to an arbitrary object, we again
  # validate that the resulting object is indeed a metaframe:
  assertthat::assert_that(is_metaframe(mf))
  mf
}

#' @rdname metaframe
#'
#' @param value a metaframe
#'
#' @export

set_metaframe <- function(x, value) {
  assertthat::assert_that(is_metaframe(value))
  attr(x, "metaframe") <- value
  x
}

#' @rdname metaframe
#'
#' @export

`metaframe<-` <- set_metaframe

#' @describeIn metaframe determines whether x has a metaframe
#'
#' @export

has_metaframe <- function(x) {
  !is.null(tryCatch(metaframe(x), error = function(e) NULL))
}
