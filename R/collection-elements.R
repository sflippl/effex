#' Keys
#'
#' Keys are defined as columns of a table resp. arguments of a function that
#' uniquely determine specific observations resp. the function's return. They
#' simplify management of a collection as the canonical way of splitting up data
#' frames.
#'
#' @param x an R object
#'
#' @return The keys of x.
#'
#' @export

key <- function(x) {
  UseMethod("key")
}

#' @describeIn key
#'
#' @export

key.collection_df <- function(x) {
  attr(x, "key")
}

#' @describeIn key
#'
#' @export

key.function <- function(x) {
  key <- attr(x, "key")
  if(is.null(key)) return(new_key(names(formals(x))))
  else return(key)
}

#' @describeIn key
#'
#' @export

key.default <- function(x) {
  message("Objects of the class ", class(x), " cannot have a key.",
          " Only collection_dfs and functions have keys.\n")
  NULL
}

#' @describeIn key
#'
#' Does x have a key?
#'
#' @export

has_key <- function(x) key(x) != new_key(NULL)


#' @describeIn key
#'
#' @param key character vector. Each element must be a column of the data frame
#' resp. an argument of the function.
#'
#' @return x with new key `key`
#'
#' @export

change_key <- function(x, key) {
  UseMethod("change_key")
}

#' @describeIn key
#'
#' @export

change_key.collection_df <- function(x, key) {
  key <- as_key(key)
  assertthat::are_equal(length(key), 1L)
  cl <- class(x)
  if(!is.null(key[[1]])) {
    assertthat::assert_that(all(key[[1]] %in% colnames(x)))
    assertthat::assert_that(
      nrow(unique(dplyr::select(as.data.frame(x), !!key[[1]]))) ==
        nrow(dplyr::select(as.data.frame(x), !!key[[1]])))
  }
  # Keys should be up front.
  x <- x[, c(key[[1]], names(x) %>% magrittr::extract(!(. %in% key[[1]]))), drop = FALSE]
  class(x) <- cl
  attr(x, "key") <- key
  x
}

#' @describeIn key
#'
#' @export

change_key.function <- function(x, key) {
  key <- as_key(sort(key))
  assertthat::assert_that(all(key[[1]] %in% names(formals(x))))
  attr(x, "key") <- key
  x
}

#' @describeIn key
#'
#' @export

change_key.default <- function(x, key) {
  stop("Objects of the class ", class(x), " cannot have a key.",
       " Only collection_dfs and functions have keys.\n")
}

#' @describeIn key
#'
#' @export

`key<-` <- function(x, value) {
  change_key(x, value)
}

#' Collection data frames
#'
#' Collection data frames are possible elements of a collection (represented by
#' the virtual class `collection_el`).
#'
#' @param df a data frame that will be transformed to a collection data frame.
#' @param key the key of the collection data frame. Needs to consist of uniquely
#' identifying columns of `df`. May also be `NULL` (default setting), in which
#' case a name needs to be supplied to the collection.
#'
#' @export

collection_df <- function(df, key = new_key(NULL)) {
  assertthat::assert_that(is.data.frame(df))
  if(!is_collection_df(df))
    class(df) <- c("collection_df", "collection_el", class(df))
  key(df) <- key
  df
}

#' @rdname collection_df
#'
#' @export

is_collection_df <- function(x) inherits(x, "collection_df")
