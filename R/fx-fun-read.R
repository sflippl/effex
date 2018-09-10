#' Effex functions: Read Data
#'
#' The counterpart to the [fx_write]-functions. Recreates the original data
#' frame from any filepath columns.
#'
#' @export

fx_read <- function(data) {
  filepath_cols <- names(data)[purrr::map_lgl(data, is_filepath)]
  data_unaff <- dplyr::select(data, -!!filepath_cols)
  data_aff <- dplyr::select(data, !!filepath_cols)
  res <- c(list(data_unaff), purrr::map(data_aff, read_filepath))
  ret <- purrr::reduce(res, ~ dplyr::inner_join(.x, .y, by = "name"))
  ret
}
