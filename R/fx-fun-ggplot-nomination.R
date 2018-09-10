#' Nominations
#'
#' Nominations are essential objects in the construction of
#' [fx_ggplot()]. Their structure is a list of lists where each list contains a
#' different kind of "ggproto" object, namely:
#'
#' @description
#' * layers
#' * scales
#' * faceting specifications
#' * coordinate systems
#' * other ggproto objects
#'
#' This function is intended to be extendible via the generic `add_to_nom`.
#'
#' @param ... Layers that are nominated together or nominations that are
#' concatenated
#'
#' @export

nomination <- function(...) {
  lst <- rlang::dots_list(...)
  if(length(lst) == 0)
    return(structure(list(), class = c("nomination", "list")))
  add_to_nom(lst[[1]], nomination(!!!lst[-1]))
}

#' @describeIn nomination dispatched over `nomination_el`
#'
#' @param nomination_el a new nomination element. Has to be a "ggproto" object.
#' @param nomination a nomination
#'
#' @export

add_to_nom <- function(nomination_el, nomination) UseMethod("add_to_nom")

#' @rdname nomination

add_to_nom.Layer <- function(nomination_el, nomination) {
  nomination[["layer"]] <- c(nomination[["layer"]], list(nomination_el))
}

#' Access functions to a nomination
#'
#' These functions provide access to resp. change the different kinds of layers
#' of a nomination.
#'
#' @name nom_access

NULL

#' @describeIn nom_access all layers (class "Layer")
#'
#' @export

nom_layers <- function(nomination) {
  ret <- nomination[["layers"]]
  if(is.null(ret)) return(list())
  ret
}

#' @rdname nom_layers
#'
#' @export

`nom_layers<-` <- function(nomination, value) {
  purrr::walk(value, ~ assertthat::assert_that(inherits(., "Layer")))
  nomination[["layers"]] <- value
  nomination
}

#' @rdname nomination

add_to_nom.Layer <- function(nomination_el, nomination) {
  nom_layers(nomination) <- c(nom_layers(nomination), list(nomination_el))
  nomination
}

#' @describeIn nom_access all facets (class "Facet")
#'
#' @export

nom_facets <- function(nomination) {
  ret <- nomination[["facets"]]
  if(is.null(ret)) return(list())
  ret
}

#' @rdname nom_facets
#'
#' @export

`nom_facets<-` <- function(nomination, value) {
  purrr::walk(value, ~ assertthat::assert_that(inherits(., "Facet")))
  nomination[["facets"]] <- value
  nomination
}

#' @rdname nomination

add_to_nom.Facet <- function(nomination_el, nomination) {
  nom_facets(nomination) <- c(nom_facets(nomination), list(nomination_el))
  nomination
}

#' @describeIn nom_access all scales (class "Scale")
#'
#' @export

nom_scales <- function(nomination) {
  ret <- nomination[["scales"]]
  if(is.null(ret)) return(list())
  ret
}

#' @rdname nom_scales
#'
#' @export

`nom_scales<-` <- function(nomination, value) {
  purrr::walk(value, ~ assertthat::assert_that(inherits(., "Scale")))
  nomination[["scales"]] <- value
  nomination
}

#' @rdname nomination

add_to_nom.Scale <- function(nomination_el, nomination) {
  nom_scales(nomination) <- c(nom_scales(nomination), list(nomination_el))
  nomination
}

#' @describeIn nom_access all coordinate systems (class "Coord")
#'
#' @export

nom_coords <- function(nomination) {
  ret <- nomination[["coords"]]
  if(is.null(ret)) return(list())
  ret
}

#' @rdname nom_coords
#'
#' @export

`nom_coords<-` <- function(nomination, value) {
  purrr::walk(value, ~ assertthat::assert_that(inherits(., "Coord")))
  nomination[["coords"]] <- value
  nomination
}

#' @rdname nomination

add_to_nom.Coord <- function(nomination_el, nomination) {
  nom_coords(nomination) <- c(nom_coords(nomination), list(nomination_el))
  nomination
}

#' @describeIn nom_access all other ggproto objects
#'
#' @export

nom_others <- function(nomination) {
  ret <- nomination[["others"]]
  if(is.null(ret)) return(list())
  ret
}

#' @rdname nom_layers
#'
#' @export

`nom_others<-` <- function(nomination, value) {
  purrr::walk(value, ~ assertthat::assert_that(inherits(., "ggproto")))
  nomination[["others"]] <- value
  nomination
}

#' @rdname nomination

add_to_nom.ggproto <- function(nomination_el, nomination) {
  nom_others(nomination) <- c(nom_others(nomination), list(nomination_el))
  nomination
}

#' @rdname nomination
#'
#' @param x an object
#'
#' @export

is_nomination <- function(x) inherits(x, "nomination")

#' @rdname
#'
#' @param ... layers nominations to be concatenated into one nomination
#'
#' @export

c.nomination <- function(...) {
  lst <- rlang::dots_list(...)
  purrr::walk(lst, ~ assertthat::assert_that(is_nomination(.)))
  nomination(!!!(purrr::map(lst, ~ unlist(., recursive = FALSE))) %>% unlist)
}

