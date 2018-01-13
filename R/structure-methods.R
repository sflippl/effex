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
    activate(nodes) %>%
    rbind_newcol(variables)
}

#' @rdname extend
#'
#' @export

extend_relations <- function(structure, relations = C_Relation(character(0))) {
  valid_structure_input(NULL, relations, NULL)
  attr(structure, "relations") <- attr(structure, "relations") %>%
    rbind_newcol(relations)
}

#' @rdname extend
#'
#' @export

extend_edges <- function(structure, edges = data.frame(from = numeric(0),
                                                       to = numeric(0),
                                                       name = numeric(0))) {
  valid_structure_input(NULL, NULL, edges)
  edges <- get_ref_edges(structure %>% activate(nodes),
                         attr(structure, "relations"),
                         edges)
  structure %>%
    activate(edges) %>%
    rbind(edges)
}

#' Binding rows with possible new columns

ncbind_rows <- function(...,
                         deparse.level = 1,
                         make.row.names = TRUE,
                         stringsAsFactors = default.stringsAsFactors()) {
  dfs <- list(...)
  all_names <- sapply(dfs, colnames) %>% unlist %>% unique
  dfs <- lapply(dfs, function(x) {
    missing_cols <- all_names[!(all_names %in% colnames(x))]
    for(col in missing_cols) x[, col] <- ""
    x
  })
  ret <- dfs[[1]]
  for(df in seq_len(length(dfs) - 1)) {
    ret <- ret %>%
      bind_rows(dfs[[df + 1]])
  }
  ret
}
