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
<<<<<<< HEAD
#' @export
=======
#' @include
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be

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
<<<<<<< HEAD
#'
#' @export

as_architecture <- function(archie, verbose = TRUE) {
  assert_that(is.logical(verbose))
  UseMethod("as_architecture")
}

#' @export

=======

as_architecture <- function(archie, verbose = TRUE) {
  UseMethod("as_architecture")
}

>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be
as_architecture.default <- function(archie, ...) {
  stop("Invalid class for archie.")
}

<<<<<<< HEAD
#' @export

=======
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be
as_architecture.environment <- function(archie, verbose = TRUE, ...) {
  class(archie) <- c("architecture", "environment")

  attr(archie, "verbose") <- verbose

  archie
}

<<<<<<< HEAD
#' @export

=======
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be
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
<<<<<<< HEAD
#'
#' @export
=======
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be

is_architecture <- function(x) {
  inherits(x, "architecture")
}

#' @describeIn is_architecture Is the architecture verbose?
<<<<<<< HEAD
#'
#' @export
=======
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be

is_verbose <- function(archie) {
  attr(archie, "verbose")
}

#' @describeIn is_architecture Change the architecture's verbosity.
<<<<<<< HEAD
#'
#' @export

set_verbose <- function(archie, verbose) {
  archie_name <- as.character(enexpr(archie))

=======

set_verbose <- function(archie, verbose) {
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be
  prev_verbose <- is_verbose(archie)

  attr(archie, "verbose") <- verbose

  if(prev_verbose) {
<<<<<<< HEAD
    message("Changed verbosity of architecture ", archie_name,
            " to ", verbose, ".\n")
=======
    message("Changed verbosity of architecture to ", verbose, ".\n")
>>>>>>> 3e8d278c4d07908f5ff77e825d6ee16264c132be
  }

  archie
}
