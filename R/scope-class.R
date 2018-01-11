#' `Scope`
#'
#' `Scope`s define the level on which we make observation about our `Compound`s.
#' They are referenced in `Compound`s as well as many `Operator`s.
#'
#' @param name a `character`. Specifies the name of the scope.
#' @param slots a named vector. Specifies the additional slots the scope needs.
#' @param supscope `Scopes` may be stacked by providing a supscope.
#'
#' @seealso [Compound]

Scope <- function(name, slots, supscope = "Compound") {
  # If inherits is supplied, see if it is valid, i. e. an existing Scope that is
  # not Atoms.
  if(supscope != "Compound") {

  }
  setClass(name, slots = slots, contains = supscope)
}
