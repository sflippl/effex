#' @describeIn fx_info provides a statistical summary table
#'
#' @param statistics a character vector or a list of characters and functions.
#' Function slots must have a name. Functions can accept arguments from
#' metaframe columns. The corresponding column will be fed in first and there
#' will be an additional data column with the entire data. All arguments but
#' the column will only be fed into functions, not characters.
#'
#' @export

fxe_info.fxd_info_stats <- function(data, topic, statistics, ...) {
  mf <- metaframe(data)
  ret <- dplyr::tibble(name = mf$name)
  stat_len <- length(statistics)
  names <- stat_names(statistics)
  for(i_stat in seq_len(stat_len)) {
    tmp_col <- purrr::map(
      seq_len(nrow(mf)),
      function(i_row) {
        tmp_mf <- mf[i_row, ]
        stat_fun <- statistics[[i_stat]]
        if(is.function(stat_fun))
          stat_args <- c(
            list(data[[tmp_mf$name]], data = data),
            lst_mf_args(tmp_mf),
            rlang::dots_list(...))
        else stat_args <- list(data[[tmp_mf$name]])
        do.call(stat_fun, stat_args)
      }
    )
    # Only simplify if every element has length one. If not then the vector
    # does not correspond to the names anymore.
    # However if every list has the same length and the same names, we again
    # expand.
    len <- purrr::map_int(tmp_col, length)
    nam <- unique(purrr::map(tmp_col, ~ names(.)))
    if(length(unique(len)) == 1) {
      if(unique(len) == 1) ret[[names[[i_stat]]]] <- purrr::simplify(tmp_col)
      else if(length(nam) == 1 && !is.null(nam[[1]]) &&
              !any(duplicated(nam[[1]])))
        ret <- dplyr::bind_cols(
          ret,
          tmp_col %>% purrr::map_dfr(
            ~ t(.) %>% dplyr::as_tibble() %>%
              magrittr::set_names(paste0(names[[i_stat]], ": ", nam[[1]])))
        )
      else ret[[names[[i_stat]]]] <- tmp_col
    }
    else ret[[names[[i_stat]]]] <- tmp_col
  }
  ret
}

#' Names of statistics
#'
#' Get the names of the `statistics` in [fxe_info.fxd_info_stats()].

stat_names <- function(statistics) {
  stat_len <- length(statistics)
  names <- purrr::map_chr(
    seq_len(stat_len),
    function(i) {
      nam <- names(statistics)[i]
      if(is.null(nam) || nam == "") {
        assertthat::assert_that(
          is.character(statistics[[i]]),
          msg = "Unnamed elements of `statistics` must be characters.")
        return(statistics[[i]])
      }
      nam
    }
  )
  assertthat::assert_that(
    !any(duplicated(names)),
    msg = "Names for statistics must be unique.")
  names
}
