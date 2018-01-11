#' @include scope-class.R
#' @import methods

NULL

# Compound----------------------------------------------------------------------

#' `Compound`
#'
#' `Compound`s are the objects we observe. They are most effective in
#' combination with `System`s which pose stronger conditions on their values.
#' Without a `System`, a `Compound` is little more than a template.

# setClass("Compound", contains = "data.table")

# System------------------------------------------------------------------------

#' Systems
#'
#' `System`s are the structure in which we observe `Compound`s. `Compound`s can
#' be the combination of several different `System`s. Systems are actually class
#' generators.
#'
#' A similar framework to the bare definition of S4 classes is recommended. It
#' may be useful to define a new class for the slot value if there are more than
#' Atoms.
#'
#' @examples
#' .Humans <- System(name = "Humans", slots = c(id = "numeric",
#'                                             name = "character",
#'                                             height = "numeric"))
#' Humans <- function(id = NA_real_, name = NA_character_, height = NA_real_) {
#'   # Humans' id is unique but we trust that the name-height-combination is
#'   # unique as well.
#'   if(is.na(id) & (is.na(name) | is.na(height))) stop("Mix-up possible.")
#'   .Humans(id = id, name = name, height = height)
#' }
#' # We may now extend our definition of humans to collections of humans by
#' # considering Atoms.

setClass("System", contains = "classGeneratorFunction")

#' @param name Name of the class that is defined. The function throws an error
#' if the class has already been defined.
#' @param slots A named character vector. The slots the class should have.
#' @param contains a character vector. Possible inheritances.
#' @param listable logical. Should the class have a corresponding
#'     "<classname>_list"-class. This allows you to define listable functions.
#'
#' @export
#'
#' @rdname System-class

System <- function(name, slots, contains = NULL, listable = TRUE) {
  # throw an error if the class already exists
  if(!is_free_classname(name)) stop("A class with that name has already been
                                    defined.")
  ret <- new("System",
             setClass(name, slots = slots, contains = c("Compound", contains)))
  extend_listmethods(name)
  ret
}
