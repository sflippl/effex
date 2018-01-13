#' Extend a structure
#'
#' Extend a structure with new content, new relations or new edges.
#'
#' @param structure a structure to be extended
#' @inheritParams Structure
#'
#' @export

extend <- function(structure,
                   variables = Content(character(0)),
                   relations = C_Relation(character(0)),
                   edges = data.frame(from = numeric(0),
                                      to = numeric(0),
                                      name = numeric(0))) {
  structure %>%
    extend_variables(variables) %>%
    extend_relations(relations) %>%
    extend_edges(edges)
}

#' @rdname extend
#'
#' @export

extend_variables <- function(structure, variables = Content(character(0))) {
  valid_structure_input(variables, NULL, NULL)
  structure %>%
    bind_nodes(variables)
}

#' @rdname extend
#'
#' @export

extend_relations <- function(structure, relations = C_Relation(character(0))) {
  valid_structure_input(NULL, relations, NULL)
  attr(structure, "relations") <- attr(structure, "relations") %>%
    bind_rows(relations)
  structure
}

#' @rdname extend
#'
#' @export

extend_edges <- function(structure, edges = data.frame(from = numeric(0),
                                                       to = numeric(0),
                                                       name = numeric(0))) {
  valid_structure_input(NULL, NULL, edges)
  edges <- get_ref_edges(structure %>% activate(nodes) %>% as.data.frame,
                         attr(structure, "relations"),
                         edges)
  structure %>%
    bind_edges(edges)
}

