#' Effex functions: Defaults for the metaframe
#'
#' The `fx_default`-family consists of functions with a consistent interface that
#' that sets a default value for the specified metaframe column. Their intention
#' is an explicit standard mode
#' which allows to concentrate on changing special cases only.
#' `fx_default` itself wraps around these functions by modifying the metaframe
#' for any number of metaframe columns. This is, of course only useful if
#' modifications need not be applied.
#'
#' @section Extension Mechanism:
#' `fx_default` can be extended by adding a method to the S3 generic
#' `fxe_default`.
#' It is recommended to specify the particular method following the scheme
#' `fx_default_<column name>` to make it easier to use effex default outside an
#' entire data frame. The new method must conform to the [fxd()] naming
#' mechanism where the task is "default" and the column name is the subclass.
#'
#' @param data The dataframe with a metaframe attribute
#' @param columns `character`. The columns of the metaframe that should be
#' provided.
#' @param overwrite `logical`. Should existing columns be overwritten? If `TRUE`
#' might also delete columns without compatible method.
#' @param mustWork If no compatible method is found, should the method return
#' `NULL` or throw an error?
#' @param ... Parameters to give on to the methods of [fxe_default()]
#'
#' @export

fx_default <- function(data, columns = character(0), ...,
                     overwrite = FALSE, mustWork = TRUE) {
  if(!has_metaframe(data)) metaframe(data) <- new_metaframe(name = names(data))
  else metaframe(data) <-
      metaframe(data) %>%
      dplyr::filter(name %in% names(data)) %>%
      as_metaframe()
  mf <- metaframe(data)
  assertthat::assert_that(is.character(columns))
  for(col in columns) {
    if(!overwrite & col %in% names(mf)) next
    mf[[col]] <- fxi_default(data, col, ..., mustWork = mustWork)
  }
  metaframe(data) <- mf
  data
}

fxi_default <- function(data, col, ...) {
  fxe_default(data, metaframe(data), fxd("default", col), ...)
}

#' @rdname fx_default
#'
#' @export

fxe_default <- function(data, mf, col,
                        ..., mustWork = TRUE) {
  UseMethod("fxe_default", col)
}

#' @describeIn fx_default returns an error if no previous method has been found.
#'
#' @examples
#' fxe_default(data.frame(),
#'             new_metaframe(name = character(0)),
#'             col = fxd("default"),
#'             mustWork = FALSE)
#' \donttest{
#' fxe_default(data.frame(),
#'             new_metaframe(name = character(0)),
#'             col = fxd("default"))
#' }
#'
#'
#' @export

fxe_default.fxd_default <- function(data, mf, col, ..., mustWork = TRUE) {
  if(mustWork) {
    stop("No default function defined for column name ", fxd_subclass(col))
  }
  NULL
}
