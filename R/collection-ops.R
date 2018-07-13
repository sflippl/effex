#' Collection Operators
#'
#' @name collection_ops

NULL

#' @rdname collection_ops
#'
#' @export

`[.collection` <- function(x, i) {
  nm <- namekey(x)
  x <- NextMethod()
  if(!is_collection(x)) class(x) <- c("collection", "list")
  attr(x, "namekey") <- nm[i, ]
  x
}
