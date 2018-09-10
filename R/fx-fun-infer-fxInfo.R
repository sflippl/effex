#' Effex Functions: Infer the column `fxInfo_name`
#'
#' `fxInfo_name` is a more human-readable title of the particular variable than
#' `name`. Nevertheless, inference simply assigns the name to it.
#'
#' @inheritParams fx_infer_fxGeom_class
#'
#' @export

fx_infer_fxInfo_name <- function(data, mf) {
  mf$name %>% stringr::str_replace_all(stringr::coll("_"), " ")
}

#' @rdname fxext_infer
#'
#' @export

fxext_infer.fxd_infer_fxInfo_name <- function(data, mf, col, ...)
  fx_infer_fxInfo_name(data, mf)
