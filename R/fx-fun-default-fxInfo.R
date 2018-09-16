#' Effex Functions: Default for the column `fxInfo_name`
#'
#' `fxInfo_name` is a more human-readable title of the particular variable than
#' `name`. Nevertheless, inference simply assigns the name to it and replaces
#' underscores with spaces.
#'
#' @param data A data frame
#' @param mf The metaframe. The metaframe of `data` is set as default.
#'
#' @inheritParams fx_default_fxGeom_class
#'
#' @example
#' fx_default_fxInfo_name(
#'   data.frame(Column1 = 1:10, Column_with_underscore = 1:10)
#' )
#'
#' @export

fx_default_fxInfo_name <- function(data, mf = metaframe(fx_default(data))) {
  mf$name %>% stringr::str_replace_all(stringr::coll("_"), " ")
}

#' @desribeIn fxe_default infers the column `fxInfo_name` via
#' [fx_default_fxInfo_name()]
#'
#' @export

fxe_default.fxd_default_fxInfo_name <- function(data, mf, col, ...)
  fx_default_fxInfo_name(data, mf)
