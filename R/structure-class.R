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
#' @details The implementation is highly inefficient as far as I can tell but
#' we will change that in the future.

Structure.relation <- function(relation, subject, object, ...) {
  # Get a list of the unique subjects and objects, respectively the relations.

  variables <- c(subject, object)
  class(variables) <- class(subject)
  variables <- Set_list(c(subject, object))

  relations <- Set_list(relation)

  structure <- data.frame(0, nrow = length(variables), ncol = length(variables))

  attr(structure, "variables") <- variables
  attr(structure, "relations") <- relations
  class(structure) <- "structure"

  # Check for every subject the relations.

  for(i in seq_len(subject)) {
    structure <- structure %>%
      add_relation(relation[[i]], subject[[i]], object[[i]])
  }

  structure
}
