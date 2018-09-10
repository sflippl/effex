#' FxInfer: S3 class for inferring columns via [fx_infer()]
#'
#' `new_FxInfer` creates a very simple class that only works via its class
#' attribute.
#'
#' @param subclass The class of the object.
#'
#' @return An empty list of class `c(<subclass>FxInfer, "FxInfer")`
#'
#' @export

new_FxInfer <- function(subclass = NULL) {
  assertthat::assert_that(length(subclass) <= 1)
  if(is.null(subclass)) return(structure(list(), class = "FxInfer"))
  structure(list(), class = c(paste0(subclass, "FxInfer"), "FxInfer"))
}

#' @rdname new_FxInfer
#'
#' @param x an object.
#'
#' @export

is_FxInfer <- function(x) {
  inherits(x, "FxInfer")
}

#' @describeIn new_FxInfer returns the specified subclass
#'
#' @param FxInfer an `FxInfer`-object.
#'
#' @export

subclass <- function(FxInfer) {
  assertthat::assert_that(is_FxInfer(FxInfer))
  subclass <- class(FxInfer) %>% stringr::str_subset("\\S+FxInfer$") %>%
    stringr::str_extract("\\S+(?=FxInfer$)")
  if(length(subclass) == 0) return(NULL)
  assertthat::assert_that(length(subclass) <= 1)
  subclass
}
