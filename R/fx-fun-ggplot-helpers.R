#' Required aesthetics of a layer
#'
#' Concatenates the required aesthetics of the geom, stats and position of a
#' layer.
#'
#' This function is not very helpful because the required aesthetics are
#' overestimated, see `GeomBoxplot`. It will thus not be employed for now.
#'
#' @param layer a layer.
#'
#' @seealso [ggplot2::layer()]

required_aes <- function(layer) {
  unique(c(
    layer$stat$required_aes,
    layer$geom$required_aes,
    layer$position$required_aes
  ))
}

#' NULL helper
#'
#' This function is a simple helper that replaces `list(NULL)` with `NULL` and
#' `NULL` with `default`. It allows an easier passing of arguments in the
#' external functions.

null_helper <- function(value, default) {
  if(is.null(value)) return(default)
  if(identical(value, list(NULL))) return(NULL)
  value
}
