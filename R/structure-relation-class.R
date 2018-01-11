#' Relations

is_relation <- function(x) "relation" %in% class(x)

#' @rdname is_relation
#'
#' @description C_Relations are special `content` variables that describe the
#' relationship between two `content`s
#' (possible extension further down the road).
#'
#' @param relation a character. the kind of relation
#' @param definition a short definition of the variable
#' @param ... details on the relation
#'
#' @seealso [Content()]

.C_Relation <- function(relation, definition = "", ...) {
  c_relation <- .Content(title = relation, definition = definition, ...)
  class(c_relation) <- c("atom_c_relation", "c_relation", "relation",
                         class(c_relation))
  c_relation
}

#' @rdname is_relation
#'
#' @export

C_Relation <- function(relation, definition = "", ...) {
  stopifnot(length(relation) == length(definition))
  ret <- list()
  for(i in seq_len(length(relation))) {
    ret[[i]] <- .C_Relation(relation[i], definition[i], ...)
  }
  class(ret) <- c("c_relation", "relation", "content", "variable", class(ret))
  ret
}
