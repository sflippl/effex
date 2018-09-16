#' fx_factor: A simple, downsized alternative for arbitrary objects
#'
#' Often, objects of a large size repeating themselves need to be stored in a
#' data frame. `fx_factor` provides a simple framework to work with these
#' objects in a space-saving manner - at least until the modeling and
#' visualization.
#'
#' Levels inference is more complicated for arbitrary classes. Requiring
#' [identical()](identity) may be too much (e. g.
#' `identical(ggplot2::geom_point(), ggplot2::geom_point())`) whereas [match()]
#' (matching) might be too lax (e. g.
#' `match(list(ggplot2::geom_point()), list(ggplot2::geom_point(stat = "log10"))`).
#' If in doubt, using `as_fx_factor` is recommended and `fx_factor` will return
#' an error if encoding and decoding changes the result according to `identical`
#' (with the argument `ignore.environment = TRUE`).
#'
#' Also, all elements of `x` must occur in `levels`.
#'
#' @param x a vector or a list
#' @param levels a set of possible, unique objects that may occur in x
#'
#' @examples
#'
#'
#' @export

fx_factor <- function(x, levels = unique(x)) {
  assertthat::assert_that(!any(duplicated(levels)))
  x_int <- match(x, levels)
  ret <- as_fx_factor(x_int, levels = levels)
  assertthat::assert_that(
    identical(x, fx_evaluate(ret), ignore.environment = TRUE),
    msg = "x cannot be reconstructed. See `?fx_factor"
  )
  ret
}

#' @rdname fx_factor
#'
#' @param x_int an integer vector referring to the levels
#'
#' @export

as_fx_factor <- function(x_int, levels) {
  assertthat::assert_that(is.integer(x_int),
                          length(levels) >= max(x_int, na.rm = TRUE))
  structure(x_int, levels = levels, class = "fx_factor")
}

#' @rdname fx_factor
#'
#' @export

is_fx_factor <- function(x) inherits(x, "fx_factor")

#' @rdname fx_factor
#'
#' @export

`levels<-.fx_factor` <- function(x, value) {
  as_fx_factor(x, value)
}

#' Evaluating factors
#'
#' `fx_evaluate` returns the list/vector of the original kind. It is defined as
#' a generic to make uncomplicated evaluation of a data frame possible. `[`
#' provides ordinary subsetting functions whereas `[[` automatically evaluates.
#'
#' @export

fx_evaluate <- function(x) UseMethod("fx_evaluate")

#' @rdname fx_factor
#'
#' @export

fx_evaluate.default <- function(x) x

#' @rdname fx_factor
#'
#' @export

fx_evaluate.fx_factor <- function(x) {
  levels(x)[x]
}

#' @rdname fx_factor
#'
#' @export

fx_evaluate.data.frame <- function(x) {
  has_mf <- has_metaframe(x)
  if(has_mf) mf <- fx_evaluate(metaframe(x))
  ret <- purrr::map_dfc(x, fx_evaluate)
  if(has_mf) metaframe(ret) <- mf
  ret
}

#' @rdname fx_factor
#'
#' @export

fx_evaluate.metaframe <- function(x) {
  as_metaframe(NextMethod())
}

#' @rdname fx_factor
#'
#' @export

`[.fx_factor` <- function(x, i) {
  as_fx_factor(as.integer(x)[i], levels(x))
}

#' @rdname fx_factor
#'
#' @export

`[[.fx_factor` <- function(x, i) {
  levels(x)[[i]]
}
