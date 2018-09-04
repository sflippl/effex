#' @include fx.R fxPromise.R
NULL

#' Effex Class: Geom
#'
#' The class `fxGeom` contains different information on the properties, the
#' visualization of a certain variable should have. Its main subclasses are
#' `fxContinuousGeom` and `fxDiscreteGeom`.
#'
#' @export

.fxGeom <- setClass("fxGeom", contains = "fx")

#' @rdname fxGeom-class
#'
#' @slot limits Range of a continuous variable. Default is `c(NA, NA)` as this
#' does not influence the limits.
#'
#' @export

.fxContinuousGeom <- setClass("fxContinuousGeom", contains = "fxGeom",
                              slots = c(
                                limits = "numeric"
                              ))

setClassUnion("CharNULL", c("character", "NULL"))

#' @rdname fxGeom-class
#'
#' @slot limits `character`, `numeric` or `NULL`: Values of a discrete variable.
#' Default is NULL.
#'
#' @export

.fxDiscreteGeom <- setClass("fxDiscreteGeom", contains = "fxGeom",
                            slots = c(
                              limits = "CharNULL"
                            ))

#' @rdname fxGeom-class
#'
#' @export

fxContinuousGeom <- function(limits = c(NA_real_, NA_real_)) {
  assertthat::assert_that(length(limits) == 2 & limits[1] <= limits[2])
  .fxContinuousGeom(limits = limits)
}

#' @rdname fxGeom-class
#'
#' @export

fxDiscreteGeom <- function(limits = NULL) {
  .fxDiscreteGeom(limits = limits)
}

#' Effex Promise: Geom
#'
#' This class creates `fxGeom`.
#'
#' @export

.fxpGeom <- setClass("fxpGeom", contains = "fxp",
                     slots = c(mapping = "function"))

#' @rdname fxpGeom-class
#'
#' @param mapping A function that turns one row of the indicator data frame into
#' an `fxGeom` object. Must have as its only argument `indicator_row`.
#'
#' @export

fxpGeom <- function(mapping) {
  assertthat::assert_that(names(formals(mapping)) == "indicator_row")
  .fxpGeom(mapping = mapping)
}

#' @rdname fxpGeom-class
#'
#' @export

setMethod("createFx", signature = "fxpGeom",
          function(fxp, indicator)
            purrr::map(seq_len(nrow(indicator)), ~fxp@mapping(indicator[., ])))
