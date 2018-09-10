#' Effex Functions: Default of the column `fxGeom_class`
#'
#' `fxGeom_class` specifies the class of the `fxGeom` object which [fx_ggplot()]
#' works on. `fx_default_fxGeom_class` infers the class according to the class of
#' the corresponding column.
#'
#' @param data a data frame
#' @param mf a metaframe
#'
#' @section Inference Mechanism:
#' * Numeric: [fxGeomContinuous-class](fxGeomContinuous)
#' * Character or Factor: [fxGeomDiscrete-class](fxGeomDiscrete)
#' * Simple Feature column: [fxGeomSpatial-class](fxGeomSpatial)
#' * Else: [fxGeom-class](fxGeom)
#'
#' @export

fx_default_fxGeom_class <- function(data, mf) {
  fxGeom_classes <- purrr::map_chr(
    mf$name,
    ~ dplyr::case_when(
      is.numeric(data[[.]]) ~ "Continuous",
      is.factor(data[[.]]) | is.character(data[[.]]) ~ "Discrete",
      inherits(data[[.]], "sfc") ~ "Spatial",
      TRUE ~ ""
    )
  )
  fxGeom_classes
}

#' @export
#'
#' @rdname fxext_default

fxext_default.fxd_default_fxGeom_class <- function(data, mf, col, ...)
  fx_default_fxGeom_class(data, mf)

