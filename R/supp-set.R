#' Sets
#'
#' Creates a list with class `Set` from the input which contains unique
#' elements.
#'
#' @param ... the elements of the set.
#'
#' @export

Set <- function(...) {
  Set_list(list(...))
}

#' @rdname Set
#'
#' @param list_inp the elements of the set as a list.
#'
#' @export

Set_list <- function(list_inp) {
  ret <- list()
  for(inp in list_inp) {
    # Check if the element has already been added.
    if(!(inp %el% ret))
      ret[[length(ret) + 1]] <- inp
  }
  class(ret) <- c("Set", class(inp))
  ret
}

#' Set operations
#'
#' Supplementary set operations
#'
#' @param x a possible element
#' @param S a set.
#'
#'
#' @export

match_els <- function(x, S, nomatch = 0) {
  for(i in seq_len(length(S))) {
    if(all.equal(x, S[[i]]) == TRUE) return(i)
  }
  return(nomatch)
}

#' @rdname match_els
#'
#' @examples
#' match_els(Content("Intelligence"),
#' list(Content("Intelligence"), 2, Content("uzwcjgh"))

"%el%" <- function(x, S) match(x, S, nomatch = 0) > 0
