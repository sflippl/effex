#' Subsetting Collection
#'
#' Collection subsetting essentially works on different columns of variables.
#' New functions may be written according to a standard template.
#'
#' @name collection_subsetting

NULL

#' @rdname collection_subsetting
#'
#' @param ind_names Select according to ind_name
#'
#' @param cols Select according to ind_name
#'
#' @param df_names Select according to df_name
#'
#' @param df_keys Select according to df_key
#'
#' @param nrs_lst This feature should be used with caution. It is mainly
#' available for internal purposes.
#'
#' @export
#'
#' @return A `collection`.

`[.collection` <- function(x, i) {
  variables(x) <- variables(x)[, i]
  x
}
