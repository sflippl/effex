#' Effex functions: Infer the metaframe
#'
#' The `fx_infer`-family consists of functions with a consistent interface that
#' attempts automatical inference for the specified metaframe column. They
#' return the inferred column. Their intention is an explicit standard mode
#' which allows to concentrate on changing special cases only.
#' `fx_infer` itself wraps around these functions by modifying the metaframe
#' for any number of metaframe columns. This is, of course only useful if
#' modifications need not be applied.
#'
#' `fx_infer` can be extended by adding a method to the generic `fxext_infer`.
#' It is recommended to specify the particular method following the scheme
#' `fx_infer_<column name>` to make it easier to use effex inference outside an
#' entire data frame. To achieve that `fxint_infer` creates a structure of class
#' <column name> with superclass "fx_infer" via [fxd()]
#'
#' @param data The dataframe with a metaframe attribute
#' @param columns `character`. The columns of the metaframe that should be
#' inferred.
#' @param overwrite `logical`. Should existing columns be overwritten? If `TRUE`
#' might also delete columns without compatible method.
#' @param mustWork If no compatible method is found, should the method return
#' `NULL` or throw an error?
#' @param ... Parameters to give on to the methods of [fxext_infer()]
#'
#' @export

fx_infer <- function(data, columns = character(0), ...,
                     overwrite = FALSE, mustWork = TRUE) {
  if(!has_metaframe(data)) metaframe(data) <- new_metaframe(name = names(data))
  else metaframe(data) <-
      dplyr::filter(metaframe(data), name %in% names(data)) %>% as_metaframe()
  mf <- metaframe(data)
  assertthat::assert_that(is.character(columns))
  for(col in columns) {
    if(!overwrite & col %in% names(mf)) next
    mf[[col]] <- fxint_infer(data, col, ..., mustWork = mustWork)
  }
  metaframe(data) <- mf
  data
}

#' @rdname fx_infer

fxint_infer <- function(data, col, ...) {
  fxext_infer(data, metaframe(data), fxd("infer", col), ...)
}

#' @rdname fx_infer
#'
#' @export

fxext_infer <- function(data, mf, col, ..., mustWork = TRUE) {
  UseMethod("fxext_infer", col)
}

#' @rdname fx_infer
#'
#' @export

fxext_infer.fxd_infer <- function(data, mf, col, ..., mustWork = TRUE) {
  if(mustWork) {
    stop("No inference function defined for column name ", fxd_subclass(col))
  }
  NULL
}
