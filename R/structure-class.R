#' Structure
#'
#' Structures are collections of variables and relations between them. Saved in
#' the form `relation` `content`.
#'
#' @param relation The relation.
#'
#' @export

Structure <- function(relation, ...) {
  UseMethod("Structure", relation)
}

#' @rdname Structure

Structure.default <- function(relation, ...) {
  error("Provide an appropriate relation")
}

#' @rdname Structure
#'

Structure.relation <- function(variables) {

}
