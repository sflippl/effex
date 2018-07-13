#' Join Collections
#'
#' `join` is a canonical form of the `*_join` set of functions. It has no `by`
#' argument as it joins by the keys of `x` and `y` and only allows identical
#' values of identical variables for identical keys.
#'
#' @param x a `collection_df`.
#' @param y a `collection_df` with identical keys as `x`
#' @inheritParams dplyr::full_join
#'
#' @export

join <- function(x, y, copy = FALSE, ...) {
  UseMethod("join")
}

#' @rdname join
#'
#' @export

join.collection_df <- function(x, y, copy = FALSE, ...) {
  if(is_collection(y)) return(join(y, x, copy = copy, ...))
  if(!is_collection_df(y)) stop("Cannot join collection_df with ",
                                paste(class(y), collapse = "/"),
                                ".\n")
  assertthat::assert_that(setequal(key(x), key(y)), has_key(x))
  ret <-
    dplyr::full_join(x, y, copy = copy, by = key(x), suffix = c("", ".y"), ...)
  # We retain y's columns to check for identical column values and get rid of
  # them afterwards.
  possible_conflicts <- names(x) %>%
    magrittr::extract(. %in% names(y) & !(. %in% key(x)))
  for(nam in possible_conflicts) {
    assertthat::assert_that(all(ret[[nam]] == ret[[paste0(nam, ".y")]],
                                na.rm = TRUE))
    ret[[nam]] <- dplyr::if_else(!is.na(ret[[nam]]),
                                 ret[[nam]],
                                 ret[[paste0(nam, ".y")]])
  }
  dplyr::select(ret, -c(glue::glue(possible_conflicts, ".y")))
}

#' @describeIn join Joins a `collection` with a `collection` or a
#' `collection_df`. If `y` is a `collection_df` a `name` for `y` can be
#' specified.
#'
#' @param name a name for `y`. If unspecified, `join` sets the name empty (i. e.
#' ""). If `y` is a collection `name = NULL` does not affect the names but
#' `name = ""` does.
#'
#' @export

join.collection <- function(x, y, copy = FALSE, name = NULL, ...) {
  if(is_collection(y)) {
    if(!is.null(name)) names(y) <- name
    if(length(y) != 0) {
      x <-
        join(join(x, y[-length(y)]), y[[length(y)]], name = names(y)[length(y)])
    }
  }
  else if(!is_collection_df(y)) stop("Cannot join collection with ",
                                paste(class(y), collapse = "/"),
                                ".\n")
  else {
    assertthat::assert_that(
      length(name) <= 1,
      has_key(y) | length(name) == 1
    )
    if(!is.null(name)) assertthat::assert_that(has_key(y) | name != "")
    nm <- namekey(x)
    new_nm <- dplyr::tibble(
      name = dplyr::if_else(is.null(name), "", name),
      key = list(key(y))
    )
    k <- match(as.list(data.frame(t(new_nm))),
               as.list(data.frame(t(nm))))
    if(is.na(k)) {
      x[[length(x) + 1]] <- y
      attr(x, "namekey") <- dplyr::bind_rows(nm, new_nm)
    }
    else {
      x[[k]] <- join(x[[k]], y)
    }
  }
  x
}
