#' Get variable
#'
#' `get_variable` provides a framework for reading in indicators.
#'
#' @export

get_variable <- function(collection, indicators) {
  assertthat::assert_that(is_collection_df(indicators))
  assertthat::assert_that(key(indicators) == key("ind_name"))
  if(nrow(indicators) == 0) return(collection)
  indicators <- indicators %>%
    tidyr::nest(-raw_source, .key = "inds")
  added_ind <- indicators$inds[[1]]
  class(added_ind) <- c(indicators$raw_source[1], class(added_ind))
  added_ind %>%
    get_raw_variable(collection = collection) %>%
    tidy(indicators = added_ind)
    clean() %>%
    join(collection, .) %>%
    get_variable(tidyr::unnest(indicator[-1, ]))
}

#' @rdname get_variable
#'
#' @export

get_raw_variable <- function(indicators, collection) {
  UseMethod("get_raw_variable")
}

#' @rdname get_variable
#'
#' @export

tidy <- function(raw_variable, indicators) {
  UseMethod("tidy")
}

#' @rdname get_variable
#'
#' @export

clean <- function(x, indicator = NULL, ...) {
  UseMethod("clean")
}

#' @export

clean.collection <- function(x, indicator = NULL, ...) {
  for(i in 1:length(x)) {
    x[[i]] <- clean(x[[i]],
                    indicator =
                      if(is.null(indicator)) x[[key = "indicator"]]
                    else indicator)
  }
  x
}

#' @export

clean.collection_df <- function(x, indicator) {
  for(col in names(x)) {
    x[["col"]] <- clean(x[["col"]], filter(indicator, sapply(ind_cols,
                                                    function(x) col %in% x,
                                                    logical(1))))
  }
}

#' @export

clean.default <- function(x, indicator) x
