#' Relations

is_relation <- function(x) "relation" %in% class(x)

#' @rdname Relations
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
#'
#' @export

C_Relation <- function(relation, definition = "", ...) {
  c_relation <- Content(title = relation, definition = definition, ...)
  class(c_relation) <- c("c_relation", "relation", class(c_relation))
}
