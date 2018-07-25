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
  assertthat::assert_that(key(x) == key(y), has_key(x))
  ret <-
    dplyr::full_join(x, y, copy = copy, by = key(x)[[1]], suffix = c("", ".y"), ...)
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
    vars <- variables(x)
    n_vars <- variable(
      ind_name = names(y), col = names(y),
      df_name = dplyr::if_else(is.null(name), "", name),
      df_key = key(y),
      is_key = names(y) %in% key(y)[[1]], nr_lst = length(x) + 1
    )
    if(is.null(name)) n_name <- ""
    else n_name <- name
    compatible_vars <- dplyr::filter(vars, df_name == n_name,
                                     df_key == key(y))
    lst_nr <- unique(compatible_vars$nr_lst)
    assertthat::assert_that(length(lst_nr) <= 1)
    if(length(lst_nr) == 0) {
      lst_nr <- length(x) + 1
      x[[lst_nr]] <- y
    }
    else x[[lst_nr]] <- join(x[[lst_nr]], y)
    attr(x, "variables") <- bind_rows(
      variables(x),
      variable(ind_name = names(y), col = names(y), df_name = n_name,
               df_key = key(y), is_key = names(y) %in% key(y)[[1]],
               nr_lst = lst_nr)
    )
  }
  x
}
