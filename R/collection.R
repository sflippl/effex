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
    attr(ret, "namekey") <- dplyr::tibble(name = names(inp), key = list(key(x)))
    return(ret)
  }
  else {
    ret <- structure(list(), class = c("collection", "list"))
    attr(ret, "namekey") <- dplyr::tibble(name = character(0), key = list())
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

#' rdname collection
#'
#' @param x an object
#'
#' @export

is_collection <- function(x) {
  inherits(x, "collection")
}

#' Attributes of Collections
#'
#' Names of collections are only unique in combination with their keys. In many
#' cases, it is sensible to avoid names and only use keys.
#'
#' @export

namekey <- function(x) {
  UseMethod("namekey")
}

#' @export

namekey.collection_df <- function(x) {
  stop("Names do not belong to collection_dfs. Only keys do.")
}

#' @export

namekey.collection <- function(x) {
  attr(x, "namekey")
}

#' @rdname namekey
#'
#' @export

names.collection <- function(x) {
  namekey(x)$name
}

#' @rdname namekey
#'
#' @export

key.collection <- function(x) {
  namekey(x)$key
}

#' @rdname namekey
#'
#' @export

`names<-.collection` <- function(x, value) {
  attr(x, "namekey")$name <- value
  normalize(x)
}

#' @rdname namekey
#'
#' @export

change_key.collection <- function(x, key) {
  attr(x, "namekey")$key <- key
  for(i in seq_len(length(x))) {
    key(x[[i]]) <- namekey(x)$key[[i]]
  }
  normalize(x)
}

#' @rdname namekey
#'
#' @export

change_namekey <- function(x, value) {
  UseMethod("change_namekey")
}

#' @export

change_namekey.collection <- function(x, value) {
  names(x) <- value$name
  key(x) <- value$key
  x
}

#' @rdname namekey
#'
#' @export

`namekey<-` <- function(x, value) change_namekey(x, value)
