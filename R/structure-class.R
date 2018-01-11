#' Structure
#'
#' Structures are collections of variables and relations between them. Saved in
#' the form `subject` `relation` `object`.
#'
#' @param subject The left variable.
#'
#' @export

Structure <- function(subject, ...) {
  UseMethod("Structure", subject)
}

#' @rdname Structure

Structure.default <- function(subject, ...) {
  error("Provide an appropriate subject.")
}

#' @rdname Structure
#'
#' @param relation The relation.
#' @param object The right variable.

Structure.variable <- function(subject, relation, object, ...) {
  stopifnot(is_relation(relation), is_variable(object))

}

#' @rdname Structure

Structure.list <- function(subject, relation, object, ...) {
  if(length(relation) != length(subject) | length(object) != length(subject))
    warning("Subjects, relations and objects are not all the same length.")
  lapply(seq_len(length(subject)), function(x) {
    Structure(subject[[i]], relation[[i]], object[[i]])
  })
}
