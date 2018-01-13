#' Extend a structure
#'
#' Extend a structure with new content, new relations or new edges.
#'
#' @param structure a structure to be extended
#' @inheritParams Structure
#'
#' @export

extend <- function(structure, variables = NULL, relations = NULL, edges = NULL) {
  structure %>%
    extend_variables(variables) %>%
    extend_relations(relations) %>%
    extend_edges(edges)
}

#' @rdname extend

extend_variables <- function(structure, variables) {
  structure %>%
    activate(nodes) %>%
    rbind(variables)
}

#' @rdname extend

extend_relations <- function(structure, relations) {
  attr(structure, "relations") <- attr(structure, "relations") %>%
    rbind(relations)
}
