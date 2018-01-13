# Variable----------------------------------------------------------------------
#' Variables
#'
#' Variables are can be defined by their content or operation. This constitutes
#' the two subclasses.
#'
#' If you want to define a variable, you need to specify which subclass you have
#' by using the corresponding constructor function.
#'
#' @param x a possible variable
#'
#' @export

is_variable <- function(x) {
  "variable" %in% class(x)
}

# Content-----------------------------------------------------------------------
#' Content
#'
#' `content` is a `variable` that is defined non-statistically. It is a
#' `data.frame` with an obligatory `name` and `definition` as well as optional
#' additional columns.
#'
#' Do not set `content` as `class(x) <- "content"` but only with the constructor
#' function.
#'
#' @param name the variable name
#' @param definition a short definition of the variable
#' @param ... details on the content
#'
#' @examples
#' int <- Content(name = "Intelligence",
#'                definition = "Intelligence has been defined in many different
#'                              ways including as one's capacity for logic,
#'                              understanding, self-awareness, learning,
#'                              emotional knowledge, planning, creativity, and
#'                              problem solving. It can be more generally
#'                              described as the ability or inclination to
#'                              perceive or deduce information, and to retain it
#'                               as knowledge to be applied towards adaptive
#'                              behaviors within an environment or context.",
#'                              Source = "Wikipedia",
#'                              Subject = "Psychology")
#'
#' @export

Content <- function(name, definition = rep("", length(name)), ...) {
  ret <- data.frame(name = name, definition = definition, ...)
  class(ret) <- c("content", "variable", class(ret))
  ret
}

#' @rdname Content
#'
#' @export

is_content <- function(x) {
  if(!is_variable(x))
    warning("There is something wrong! Content should be a variable")
  "content" %in% class(x)
}
