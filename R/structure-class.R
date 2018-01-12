#' Structure
#'
#' Structures are collections of variables and relations between them -
#' essentially graphs.
#'
#' @param variables The variables of the structure.
#'
#' @export

Structure <- function(variables, ...) {
  UseMethod("Structure", variables)
}

#' @rdname Structure

Structure.default <- function(variables, ...) {
  error("Provide an appropriate relation")
}

#' @rdname Structure
#'

Structure.variable <- function(variables, relations = NULL, edges) {
  # Check appropriate input
  assert_that(is_variable(variables),
              length(unique(variables$name)) == length(variables$name),
              is.null(relations) | is_relation(relations),
              length(unique(relations$name)) == length(relations),
              all(c("from", "to") %in% colnames(edges)))

  if(!("name" %in% colnames(edges))) edges$name <- ""
  for(varnames in c("from", "to")) {
    if(is.numeric(edges$varnames))
      assert_that(0 < min(edges$varnames),
                  max(edges$varnames) < length(variables))
    else if(is.character(edges$varnames)) {
      # get positions from names
      edges$varnames <- match(edges$varnames, variables$name)
    }
  }
  ret <- tbl_graph(nodes = variables, edges = edges)
  attr(ret, "relations") <- relations
  class(ret) <- c("structure", class(ret))
  ret
}
