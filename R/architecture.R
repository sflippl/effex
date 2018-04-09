#' Architecture
#'
#' An architecture allows you to keep your variable system clean. Within the
#' architecture, you set global parameters of your indicator system.
#'
#' Architectures allow a clear distinction between how you define your
#' variable system and what you need to load for a specific task. Just like the
#' call of `generate` in comparison to its functional definition, an
#' architecture is devoted to pure coding.
#'
#' @param verbose When the architecture's functions are called, should they
#' keep you up-to-date about what they are currently doing?
#'
#' @param ... Parameters of `new.env()`
#'
#' @export

architecture <- function(
  verbose = TRUE,
  ...
) {
  archie <- new.env(...)

  as_architecture(archie, verbose = verbose)
}

#' @rdname architecture
#'
#' @param archie an environment
#'
#' @export

as_architecture <- function(archie, verbose = TRUE) {
  assert_that(is.logical(verbose))
  UseMethod("as_architecture")
}

#' @export

as_architecture.default <- function(archie, ...) {
  stop("Invalid class for archie.")
}

#' @export

as_architecture.environment <- function(archie, verbose = TRUE, ...) {
  class(archie) <- c("architecture", "environment")

  attr(archie, "verbose") <- verbose

  archie
}

#' @export

as_architecture.architecture <- function(archie, verbose = TRUE, ...) {
  prev_verbose <- is_verbose(archie)
  attr(archie, "verbose") <- verbose

  if(prev_verbose) {
    warning("You converted an architecture into an architecture.
          I changed the metaparameters.")
  }

  archie
}

#' Architecture Infos
#'
#' These functions provide additional info about an architecture.
#'
#' @param archie an architecture
#' @param x a possible architecture
#' @param verbose logical
#'
#' @describeIn is_architecture Is x an architecture?
#'
#' @export

is_architecture <- function(x) {
  inherits(x, "architecture")
}

#' @describeIn is_architecture Is the architecture verbose?
#'
#' @export

is_verbose <- function(archie) {
  attr(archie, "verbose")
}

#' @describeIn is_architecture Change the architecture's verbosity.
#'
#' @export

set_verbose <- function(archie, verbose) {
  archie_name <- as.character(enexpr(archie))

  prev_verbose <- is_verbose(archie)

  attr(archie, "verbose") <- verbose

  if(prev_verbose) {
    message("Changed verbosity of architecture ", archie_name,
            " to ", verbose, ".\n")
  }

  archie
}
