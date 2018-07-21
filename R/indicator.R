#' Indicator Sources
#'
#' Reading in and Generating variables uses the class `IndSource`. Use this
#' class to define a new method for [get_raw_variable()]. It may depend on
#' parameters like files or formulas.
#'
#' @slot file `character`. The filename or URL for external data. Not necessary
#' / set to `character(0)`, if the indicator only works with internal data.
#' [get_raw_variable()] should work with any allowed file argument. Neither
#' [tidy()] nor [clean()] should depend on `file`.
#'
#' @exportClass IndSource
#' @export

setClass("IndSource", slots = c(file = "character"))

#' @rdname IndSource-class

setMethod("initialize", "IndSource",
          function(.Object, file = character(0), ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@file <- file
  .Object
})

#' Nested Indicator Source
#'
#' This object is created by [nest_IndSource()]. It specifies the names that
#' correspond to a specific indicator source.
#'
#' @export

setClass("NestedIndSource", slots = c(ind_source = "IndSource",
                                     names = "character"))

#' @describeIn NestedIndSource-class A list of `NestedIndSource`s. The user does
#' not have to deal with this class under normal circumstances.

setClass("NISList", slots = c(nis = "list"))

setValidity("NISList", function(object) {
  ret <- assertthat::see_if(all(sapply(object@nis,
                                       function(el) is(el, "NestedIndSource"))))
  if(ret) ret
  else attr(ret, "msg")
})

#' @rdname NestedIndSource-class

setMethod("initialize", "NestedIndSource",
          function(.Object, ind_source, names, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            .Object@ind_source <- ind_source
            .Object@names <- names
            .Object
          })

nest_indicator <- function(ind_list) {
  ret <- ind_list %>%
    lapply(function(x) attributes(x) %>%
             lapply(list) %>%
             as_tibble) %>%
    dplyr::bind_rows() %>%
    select(ind_source, name)

  # Unfortunately, dplyr cannot group by list columns.

  unique_ind_sources <- unique(ret$ind_source)

  ind_source_ids <- vapply(
    X = ret$ind_source,
    FUN = function(x) {
      id <- vapply(
        X = unique_ind_sources,
        FUN = function(y) {
          isTRUE(all.equal(x, y))
        },
        FUN.VALUE = NA
      ) %>%
        which
      assertthat::assert_that(length(id) == 1)
      id
    },
    FUN.VALUE = NA_integer_
  )

  lret <- list()

  for(i in seq_len(length(unique_ind_sources))) {
    lret <- lret %>%
      append(
        new("NestedIndSource",
            ind_source = unique_ind_sources[[i]],
            names = as.character(ret$name)[i == ind_source_ids])
      )
  }

  new("NISList", nis = lret)
}

#' Indicators
#'
#' Every element of a `collection_df` contains the class `indicator`. It
#' therefore has the minimal variable structure. Besides `name`, the slots
#' are optional and introduce some convenience.
#'
#' @slot ind_source `IndSource`. The source which is used to read in variables,
#' see [IndSource-class].
#'
#' @slot name `character`. The name of the corresponding column in the
#' `collection_df`
#'
#' @slot raw_name `character`. Gives the corresponding name in the raw variable.
#' Defaults to `name`.
#'
#' @slot aliases `character`. Alternative names.
#'
#' @slot internal `logical`. Should the prepackaged data be used via
#' [get_variable_internal()]?
#'
#' @exportClass Indicator
#' @export

setClass("Indicator", slots = c(ind_source = "IndSource",
                                name = "character",
                                raw_name = "character",
                                aliases = "character",
                                internal = "logical"))

#' @rdname `Indicator-class`

setMethod("initialize", "Indicator",
          function(.Object, ind_source, name = is(.Object),
                   raw_name = name, aliases = character(0),
                   internal = TRUE, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@name <- name
  validObject(.Object)
  .Object@ind_source <- ind_source
  .Object@raw_name <- raw_name
  .Object@aliases <- character(0)
  .Object@internal <- internal
  .Object
})

setValidity("Indicator", function(object) {
  if(length(object@name) == 1) TRUE
  else glue::glue("Name has length {length(object@name)} instead of length 1.")
})
