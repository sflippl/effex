#' @export

setOldClass(c("quosure", "formula"))

#' Effex Class: Indicator Names
#'
#' This effex class only contains a single slot: `ind_name` which is defined by
#' a quosure that defines where to find the ind_names.
#'
#' @export

.fxIndName <- setClass("fxIndName", contains = "fx",
                     slots = c(ind_name = "quosure"))

#' @rdname fxIndName-class
#'
#' @export

fxIndName <- function(ind_name) {
  .fxIndName(ind_name = enquo(ind_name))
}
