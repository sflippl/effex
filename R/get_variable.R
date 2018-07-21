#' Get Variable
#'
#' Get a variable from a list of indicators.
#'
#' @param collection The collection of variables that have already been
#' imported. On the one hand, this is a shortcut as it automatically joins
#' the different collections. On the other hand, variables like a regression
#' result make use of existing variables. Collections can only be regarded in
#' `get_variable`.
#'
#' @param indicator_names The names of the new indicators.
#'
#' @param internal If existing, should the data be read in as internal to R? If
#' `internal` is TRUE but `get_variable_internal` fails, a warning is printed.
#'
#' @param ... additional arguments to `get_raw_variable` resp.
#' `get_variable_internal`.
#'
#' @export

get_variable <- function(collection, indicator_names, internal = TRUE, ...) {
  if(internal) return(get_variable_internal(indicator, collection, ...))
  nis <- nest_indicator(indicator)
  for(i in seq_len(length(nis@nis))) {
    collection <-
      get_raw_variable(source = nis@nis[[i]]@source,
                       indicators = nis@nis[[i]]@indicator,
                       collection = collection,
                       ...) %>%
      tidy(indicator = nis@nis[[i]]@names) %>%
      clean %>%
      join(collection, .)
  }
}

#' @describeIn get_variable Use this function to generate the raw, not
#' necessarily tidy variable.
#'
#' @param source The source of the indicator.
#'
#' @param indicator_names `character`. What indicators should `get_raw_variable`
#' generate?
#'
#' @export

setGeneric("get_raw_variable",
           function(source, indicator_names, collection, ...) {
  stop(glue::glue("Source must have class indicator source, not {is(source)}."))
})

#' @exportMethod

setMethod("get_raw_variable", "IndSource",
          function(source, indicator, collection, ...) {
    stop(glue::glue(
      "No generic defined for indicator source of class {is(source)}."
    ))
})

#' @describeIn get_variable Use this function to tidy the raw variable, i. e. generate
#' a collection of variables that correspond to the indicators and optionally
#' additional keys. If `raw_variable` is a collection, tidy simply selects the
#' columns that are specified by `indicator`. Any `tidy` method should therefore
#' conclude with `tidy(result, indicator)`.
#'
#' @param raw_variable The raw_variable.
#'
#' @export

setGeneric("tidy", function(raw_variable, indicator) {
  stop(glue::glue(
    "There is no function to tidy objects of class {is(raw_variable)}."
    ))
})

#' @exportMethod

setMethod("tidy", "collection", function(raw_variable, indicator) {
  select(raw_variable, indicator)
})

#' @describeIn get_variable This function cleans the different variables. By
#' default, it is set to identity.
#'
#' @param x a column of a collection. `x` may also be a `collection_df` or a
#' `collection`. These methods are already implemented.
#'
#' @export

setGeneric("clean", function(x, indicator = NULL) {
  x
})

#' @exportMethod

setMethod("clean", signature = c(x = "collection", indicator = "ANY"),
          function(x, indicator = NULL) {
  if(!is.null(indicator))
    warning("You should not specify an indicator when cleaning a collection. ",
            "The parameter will be ignored.")
  for(i in seq_len(length(x))) {
    x[[i]] <- clean(x[[i]])
  }
  x
})

#' @exportMethod

setMethod("clean", signature = c(x = "collection_df", indicator = "ANY"),
          function(x, indicator = NULL) {
  if(!is.null(indicator))
    warning("You should not specify an indicator when cleaning a collection. ",
            "The parameter will be ignored.")
  for(i in ncol(x)) {
    x[[i]] <- clean(x[[i]], indicators(x)[[i]])
  }
  x
})
