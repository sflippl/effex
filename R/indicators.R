#' Indicators
#'
#' Indicators describe the variables of a collection. They can be specified or
#' are added automatically.
#'
#' @export

indicator <- function(ind_name = character(0), present = TRUE, ...) {
  assertthat::assert_that(is.character(ind_name))
  ret <- collection_df(dplyr::tibble(ind_name = ind_name,
                                     present = present,
                                     ...),
                       key = c("ind_name"))
  class(ret) <- c("indicator", class(ret))
  ret
}

#' @rdname
#'
#' @export

is_indicator <- function(x) inherits(x, "indicator")

#' Complete indicators
#'
#' @param collection_df The `collection_df` that must be completed.
#'
#' @param indicators The existing indicators.
#'
#' @param drop logical. Should not applicable indicators be dropped.

complete_indicators <- function(collection_df, indicators = indicator(),
                                drop = FALSE) {
  relevant_indicators <- indicators %>%
    dplyr::filter(vapply(df_key,
                         function(x) setequal(x, key(collection_df)),
                         logical(1)))
  if(drop) indicators <- relevant_indicators
  remaining_cols <- names(collection_df) %>%
    magrittr::extract(!(. %in% relevant_indicators$df_name))
  indicators <- bind_rows(mutate(indicators,
                                 present = col_name %in% names(collection_df) &
                                   vapply(df_key,
                                          function(x)
                                            setequal(x, key(collection_df)),
                                          logical(1))),
                          indicator(ind_name = remaining_cols,
                                    col_name = remaining_cols,
                                    df_name = "",
                                    df_key = list(key(collection_df)),
                                    present = TRUE))
}
