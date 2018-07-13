package_required <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(pkg, " required: install that first")
}

#' Extensions of dplyr methods
#'
#' dplyr methods work differently on `collection_df`s, as they have to retain
#' key columns and ensure that the keys still uniquely determine observations.
#' For `collection_df`s, dplyr methods therefore never drop a key column,
#' preserve the right key in the case of namechanges and make sure that the
#' other conditions on `collection_df`s are satisfied. Details can be found in
#' the return section.
#'
#' @name dplyr_extensions

NULL

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions selects the specified columns plus the key
#' columns. In the case of renamed key columns, the keys are changed
#' accordingly.

select.collection_df <- function(.data, ...) {
  package_required("dplyr")
  if(has_key(.data)) {
    selected_vars <- tidyselect::vars_select(names(.data), ...)
    selected_keys <- selected_vars %>% magrittr::extract(. %in% key(.data))
    other_keys <- key(.data) %>% magrittr::extract(!(. %in% selected_keys))
    new_key <- c(names(selected_keys), other_keys)
  }
  else {
    other_keys <- NULL
    new_key <- NULL
  }
  class(.data) <- setdiff(class(.data), c("collection_df", "collection_el"))
  ret <- dplyr::select(.data, ..., !!other_keys)
  assertthat::assert_that(length(names(ret)) == length(unique(names(ret))))
  collection_df(ret, key = new_key)
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions simply makes sure that the key is still valid.

mutate.collection_df <- function(.data, ...) {
  package_required("dplyr")
  key <- key(.data)
  ret <- NextMethod()
  collection_df(ret, key = key)
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions For now, this simply makes sure that the key is
#' still valid.

arrange.collection_df <- function(.data, ...) {
  package_required("dplyr")
  key <- key(.data)
  ret <- NextMethod()
  collection_df(ret, key = key)
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions simply makes sure that the key is
#' still valid.

filter.collection_df <- function(.data, ...) {
  package_required("dplyr")
  key <- key(.data)
  ret <- NextMethod()
  collection_df(ret, key = key)
}

join_key <- function(x, y, by = NULL) {
  package_required("dplyr")
  if(!has_key(x) | !has_key(y)) new_key <- NULL
  else {
    vars <- dplyr::common_by(by = by, x = x, y = y) %>%
      as.data.frame %>%
      dplyr::transmute(x_ = x, y_ = y) # prevent misunderstandings in filter below.
    new_key <- union(
      key(x),
      key(y) %>% magrittr::extract(!(. %in% vars$y_))
    ) %>%
      union(
        vars %>% dplyr::filter(y_ %in% key(y)) %>% magrittr::use_series(x_)
      )
  }
  new_key
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions If one data frame does not have any keys, the
#' result does not have any keys either. If both data frames have keys, the
#' result has both data frame's keys as their keys (if the `by` argument affects
#' these keys, the results are appropriately changed). This always results in
#' unique keys.

inner_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    new_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = new_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `inner_join()`

left_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    new_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = new_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `inner_join()`

right_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    new_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = new_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `inner_join()`

full_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    new_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = new_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions `y` does not necessarily have to be a
#' `collection_df` as it is a filtering condition. The keys of `x` are retained.

semi_join.collection_df <- function(x, y, by = NULL, copy = FALSE, ...) {
    ret <- NextMethod()
    collection_df(ret, key = key(x))
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `semi_join`.

anti_join.collection_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  ret <- NextMethod()
  collection_df(ret, key = key(x))
}
