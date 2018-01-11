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
#' @import dplyr
#'
#' @export

is_variable <- function(x) {
  "variable" %in% class(x)
}

# Content-----------------------------------------------------------------------
#' Content
#'
#' `content` is a `variable` that is defined non-statistically. It therefore
#' consists of a character vector carrying the definition and possible comments
#' carried as attributes.
#'
#' Do not set `content` as `class(x) <- "content"` but only with the constructor
#' function.
#'
#' @param title the variable name
#' @param definition a short definition of the variable
#' @param ... details on the content
#'
#' @examples
#' int <- Content(title = "Intelligence",
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

Content <- function(title, definition = "", ...) {
  title <- paste(title)
  definition <- paste(definition)
  attribs <- list(Definition = definition, ...)
  content <- as.character(title)
  attributes(content) <- attribs
  class(content) <- c("content", "variable", class(content))
  content
}

#' @rdname Content
#'
#' @export

is_content <- function(x) {
  if(!is_variable(x))
    warning("There is something wrong! Content should be a variable")
  "content" %in% class(x)
}
