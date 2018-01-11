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
#' @seealso [Content()]#' @rdname is_relation
#'
#' @export

C_Relation <- function(relation, definition = "", ...) {
  ret <- Content(relation, definition, ...)
  class(ret) <- c("c_relation", "variable", class(ret))
  ret
}
