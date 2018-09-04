#' @include fx.R fxPromise.R
NULL

#' Effex Class: Descriptions
#'
#' This effective and explicit class contains any form of descriptions. It is a
#' mapping of  A number of special fields is listed below:
#' * label: a label for the indicator, e. g. "Number of cylinders"
#'
#' @export

.fxpDescription <- setClass("fxpDescription", contains = "fxp",
                           slots = c(
                             fx = "uneval"
                           ))

#' @rdname fxpDescription-class
#'
#' @export
#'
#' @param ... The different elements as mappings.
#'
#' @examples
#' fxpDescription(labels = labels,
#'   description = purrr::map_chr(paste0("descs/", labels),
#'                                ~ readr::read_file(.))
#' )

fxpDescription <- function(...) {
  .fxpDescription(fx = aes(...))
}

#' @rdname fxpDescription-class
#'
#' @return a data frame of the different elements
#'
#' @export

setMethod("createFx",
          c(fxp = "fxpDescription"),
          function(fxp, indicator) {
            purrr::map(
              names(fxp@fx),
              function(x)
                dplyr::tibble(rlang::eval_tidy(fxp@fx[[x]],
                                               data = indicator)) %>%
                magrittr::set_colnames(x) %>%
                fxDescription(tbl = .)
            )
          })

#' @rdname fxpDescription-class
#'
#' The class `fxDescription` finally consists of a tibble.
#'
#' @slot tbl The tibble.
#'
#' @export

.fxDescription <- setClass("fxDescription",
                           contains = "fx",
                           slots = c(tbl = "tbl"))

#' @rdname fxpDescription-class
#'
#' @param tbl The tibble.
#'
#' @export

fxDescription <- function(tbl) .fxDescription(tbl = tbl)
