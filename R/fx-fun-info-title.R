#' Effex Function: Information -- Titles
#'
#' These functions provide different kinds of titles
#'
#' @param fxInfo_title `character`, `NULL` or list of both. If
#'
#' @export

fxe_info.fxd_info_title <- function(data, topic, ...) {
  mf <- metaframe(data)
  title <- purrr::map_chr(
    seq_len(nrow(mf)),
    function(i) do.call("fx_info_title",
                rlang::list2(data = data, !!!lst_mf_args(mf[i, ]), ...))
  )
  dplyr::tibble(name = mf$name, ` ` = title)
}

#' @rdname fxe_info.fxd_info_title
#'
#' @export

fx_info_title <- function(
  data, name, ...,
  fxInfo_title = NULL,
    fxInfo_title_fun = NULL,
      fxInfo_name,
      fxInfo_unit = NULL,
      fxInfo_title_stats = character(0),
        fxInfo_title_na.show = NULL,
          fxInfo_title_na.show.threshold = 1L,
        fxInfo_title_n.show = FALSE,
      fxInfo_title_unit.show = FALSE) {
  if(!is.null(fxInfo_title)) return(fxInfo_title)
  if(!is.null(fxInfo_title_fun)) {
    title <- do.call(
      fxInfo_title_fun,
      rlang::list2(fxInfo_name = fxInfo_name, fxInfo_unit = fxInfo_unit,
                   fxInfo_title_na.show = fxInfo_title_na.show,
                   fxInfo_title_na.show.threshold =
                     fxInfo_title_na.show.threshold,
                   fxInfo_title_stats = fxInfo_title_stats,
                   fxInfo_title_unit.show = fxInfo_title_unit.show, ...))
    return(title)
  }
  if(is.null(fxInfo_title_na.show))
    fxInfo_title_na.show <-
      sum(is.na(data[[name]])) >= fxInfo_title_na.show.threshold
  if(fxInfo_title_unit.show & !is.null(fxInfo_unit))
    suff_unit <- glue::glue(" [{fxInfo_unit}]")
  else suff_unit <- ""
  fxInfo_title_stats <- c(
    if(fxInfo_title_na.show) list(NAs = function(x, ...) sum(is.na(x)))
    else NULL,
    if(fxInfo_title_n.show) list(n = "length") else NULL,
    as.list(fxInfo_title_stats))
  if(length(fxInfo_title_stats) != 0)
    stats <- fx_info(dplyr::select(data, !!name), "stats",
                     statistics = fxInfo_title_stats) %>%
    fx_output("collapse") %>%
    paste0(" (", ., ")")
  else stats <- ""
  title <- glue::glue("{fxInfo_name}{stats}{suff_unit}")
  title
}
