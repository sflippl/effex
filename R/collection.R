#' Collections
#'
#' Collections are composed of collection elements (at the time only data
#' frames). Each collection can be uniquely identified by a key and/or a name,
#' where every unique combination of key and name provides a `collection_el`
#' with the respective key.
#'
#' @param ... objects of the class `collection_df`
#'
#' @export

collection <- function(...) {
  inp <- rlang::enquos(...)
  if(length(inp) > 1)
    return(join(x = do.call("collection", inp[-length(inp)]),
                y = rlang::eval_tidy(inp[[length(inp)]]),
                name = names(inp)[length(inp)]))
  else if(length(inp) == 1) {
    x <- rlang::eval_tidy(inp[[1]])
    assertthat::assert_that(has_key(x) | names(inp) != "")
    if(!is_collection_df(x))
      stop("Collection element ", inp[[1]], " of class ",
           paste(class(x), collapse = "/"), "is not a collection element.")
    ret <- structure(list(x), class = c("collection", "list"))
    attr(ret, "variables") <- variable(
      ind_name = names(x), col = names(x), df_name = names(inp),
      df_key = key(x),
      is_key = names(x) %in% key(x)[[1]], nr_lst = 1L
    )
    return(ret)
  }
  else {
    ret <- structure(list(), class = c("collection", "list"))
    attr(ret, "variables") <- variable()
    return(ret)
  }
}

repeated_row <- function(df) {
  # By transposing and transforming to a list, we can compare every list element
  # (i. e. row) and determine the first one which matches an earlier one. We
  # return all that match an earlier one but the first one is of interest (see
  # normalize).

  df %>% t %>% data.frame %>% as.list %>% match(., .) %>% {
    which(. != seq_len(length(.)))
  }
}

normalize <- function(cl) {
  assertthat::assert_that(is_collection(cl))
  nm <- namekey(cl)
  rrow <- repeated_row(nm)
  if(length(rrow) == 0) return(cl)
  else {
    return(join(cl[-rrow[1]], cl[[rrow[1]]], name = nm$name[rrow]) %>%
             normalize)
  }
}

#' @rdname collection
#'
#' @param x an object
#'
#' @export

is_collection <- function(x) {
  inherits(x, "collection")
}

#' @rdname collection
#'
#' @export

variables <- function(x) UseMethod("variables")

#' @export

variables.collection <- function(x) attr(x, "variables")

#' @rdname collection
#'
#' @export

ind_names <- function(x) {
  variables(x)$ind_name
}

#' @rdname collection
#'
#' @export

var_cols <- function(x) {
  variables(x)$col
}

#' @rdname collection
#'
#' @export

df_names <- function(x) {
  variables(x)$df_name
}

#' @export

key.collection <- function(x) {
  variables(x)$df_key
}

#' @section Collection Manipulation:
#' Most manipulations of collections are driven by actualizing the variables or
#' changing a specific element.

`variables<-` <- function(x) UseMethod("variables<-")

`variables<-.collection` <- function(x, value) {
  assertthat::assert_that(is_variable(value))
  old <- variables(x)
  unchanged <- semi_join(x, value, by = )
}
