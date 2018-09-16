#' @describeIn fx_info determines a title via [fx_info_title()]
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

#' Build up a variable title
#'
#' This function builds a title from a name and possibly certain statistics and
#' units.
#'
#' @param data a data frame, possibly with metaframe
#' @param name the internal name of the variable
#' @param fxInfo_title either a title or `NULL`. If it is not `NULL`, all other
#' parameters are irrelevant.
#' @param fxInfo_title_fun either a function which determines a title or `NULL`.
#' If it is not `NULL`, all parameters below are irrelevant.
#' @param fxInfo_name the name of the variable
#' @param fxInfo_unit the unit of the variable or `NULL`
#' @param fxInfo_title_stats the statistics that should be given in the title.
#' Number of data points and missing values are given separately.
#' @param fxInfo_title_na.show Should the number of missing values be displayed?
#' @param fxInfo_title_na.show.threshold If `fxInfo_title_na.show` is missing,
#' what is the
#' threshold number of missing values to display them. Default is 1.
#' @param fxInfo_title_n.show Should the number of data points be displayed.
#' Default is `FALSE`
#' @param fxInfo_title_unit.show Should the unit be displayed?
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
