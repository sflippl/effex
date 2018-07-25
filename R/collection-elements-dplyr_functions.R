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
#'
#' @export

select.collection_df <- function(.data, ...) {
  package_required("dplyr")
  if(has_key(.data)) {
    selected_vars <- tidyselect::vars_select(names(.data), ...)
    selected_keys <- selected_vars %>% magrittr::extract(. %in% key(.data)[[1]])
    other_keys <- key(.data)[[1]] %>%
      magrittr::extract(!(. %in% selected_keys)) %>%
      new_key
    n_key <- new_key(c(names(selected_keys), other_keys[[1]]))
  }
  else {
    other_keys <- new_key(NULL)
    n_key <- new_key(NULL)
  }
  class(.data) <- setdiff(class(.data), c("collection_df", "collection_el"))
  ret <- dplyr::select(.data, ..., !!other_keys[[1]])
  assertthat::assert_that(length(names(ret)) == length(unique(names(ret))))
  collection_df(ret, key = n_key[[1]])
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions simply makes sure that the key is still valid.
#'
#' @export

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
#'
#' @export

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
#'
#' @export

filter.collection_df <- function(.data, ...) {
  package_required("dplyr")
  key <- key(.data)
  ret <- NextMethod()
  collection_df(ret, key = key)
}

join_key <- function(x, y, by = NULL) {
  package_required("dplyr")
  if(!has_key(x) | !has_key(y)) n_key <- new_key(NULL)
  else {
    vars <- dplyr::common_by(by = by, x = x, y = y) %>%
      as.data.frame %>%
      dplyr::transmute(x_ = .data$x, y_ = .data$y)
    # prevent misunderstandings in filter below.
    n_key <- union(
      key(x)[[1]],
      key(y)[[1]] %>% magrittr::extract(!(. %in% vars$y_))
    ) %>%
      union(
        vars %>% dplyr::filter(.data$y_ %in% key(y)) %>%
          magrittr::use_series(x_)
      ) %>%
      new_key
  }
  n_key
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions If one data frame does not have any keys, the
#' result does not have any keys either. If both data frames have keys, the
#' result has both data frame's keys as their keys (if the `by` argument affects
#' these keys, the results are appropriately changed). This always results in
#' unique keys.
#'
#' @export

inner_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    n_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = n_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `inner_join()`
#'
#' @export

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
#'
#' @export

right_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    n_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = n_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `inner_join()`
#'
#' @export

full_join.collection_df <-
  function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
    assertthat::assert_that(is_collection_df(y))
    n_key <- join_key(x = x, y = y, by = by)
    ret <- NextMethod()
    collection_df(ret, key = n_key)
  }

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions `y` does not necessarily have to be a
#' `collection_df` as it is a filtering condition. The keys of `x` are retained.
#'
#' @export

semi_join.collection_df <- function(x, y, by = NULL, copy = FALSE, ...) {
    ret <- NextMethod()
    collection_df(ret, key = key(x))
}

#' @rdname dplyr_extensions
#'
#' @describeIn dplyr_extensions see `semi_join`.
#'
#' @export

anti_join.collection_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  ret <- NextMethod()
  collection_df(ret, key = key(x))
}

#' Bind Variable Rows
#'
#' `bind_coll_rows()` is a wrapper around [dplyr::bind_rows()] that ensures
#' appropriate treatment of variables. Furthermore key variables retain their
#' class.
#'
#' @param .id Should a key be added that distinguishes between the different
#' data frames?
#'
#' @export

bind_coll_rows <- function(..., .id = NULL) {
  x <- list(...)
  if(length(x) == 0)
    if(is.null(.id)) return(collection_df())
    else return(collection_df(dplyr::tibble(character(0)) %>%
                                magrittr::set_names(.id), key = .id))
  assertthat::assert_that(
    all(vapply(x, is_collection_df, logical(1)))
  )
  key <- key(x[[1]])
  assertthat::assert_that(all(vapply(x, function(x) key(x) == key, logical(1))),
                          msg = "Keys are not identical.")
  ret <-  bind_rows(..., .id = .id)
  if(is.null(.id)) new_key <- key
  else new_key <- new_key(c(key[[1]], .id))
  for(nam in names(ret)) {
    if(nam %in% new_key[[1]]) ret[[nam]] <- as_key(ret[[nam]])
  }
  key(ret) <- new_key
  ret
}
