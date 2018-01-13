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
#'
#' @export

Structure.default <- function(variables, ...) {
  error("Provide an appropriate variable")
}

#' @rdname Structure
#' @param relations relations that extend the structure
#' @param edges new connection between contents and relations
#' @export

Structure.variable <- function(variables, relations, edges) {
  # Check appropriate input
  valid_structure_input(variables, relations, edges)

  edges <- get_ref_edges(variables, relations, edges)
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

#' Validate input to structures

valid_structure_input <- function(variables, relations, edges) {
  assert_that(is_variable(variables),
              length(unique(variables$name)) == length(variables$name),
              is.null(relations) | is_relation(relations),
              length(unique(relations$name)) == length(relations$name),
              all(c("from", "to") %in% colnames(edges)))
}

#' Let edges reference the relations and variables

get_ref_edges <- function(variables, relations, edges) {
  if(!("name" %in% colnames(edges))) edges$name <- 0
  else if(is.character(edges$name) | is.factor(edges$name)) {
    # get positions from names
    edges$name <- match(edges$name, relations$name)
  }
  else if(!is.numeric(edges$name))
    stop("edges$name is available but neither character nor a number.")
  assert_that(0 <= min(edges$name),
              max(edges$name) <= length(relations))

  if(any(edges$name == 0)) warning("There are edges without known relations")
  for(varnames in c("from", "to")) {
    if(is.numeric(edges$varnames))
      assert_that(0 < min(edges$varnames),
                  max(edges$varnames) <= length(variables))
    else if(is.character(edges$varnames) | is.factor(edges$varnames)) {
      # get positions from names
      edges$varnames <- match(edges$varnames, variables$name)
    }
    else stop("edges$", varnames, " needs to contain numbers or characters.")
  }
  edges
}
