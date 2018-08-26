#' @include effex-ggplot-layer-single_ind.R
NULL

#' @rdname fx_layer_complete_ind
#'
#' * Default: identity -- no object is added to the plot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "fxName"),
          function(fx_name, ...) return(identity))

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x is discrete: bar plot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName", x = "fxDiscreteGeom"),
          function(fx_name, ..., x) {
            function(gg) gg + ggplot2::geom_bar() +
              scale_y_continuous(limits = c(0, NA))
          })

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x is continuous: histogram
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName", x = "fxContinuousGeom"),
          function(fx_name, ..., x) {
            function(gg) gg + ggplot2::geom_histogram()
          })

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x and y are discrete: 2d heat plot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName",
                        x = "fxDiscreteGeom",
                        y = "fxDiscreteGeom"),
          function(fx_name, ..., x, y) {
            function(gg) gg + ggplot2::geom_bin2d() +
              ggplot2::scale_fill_continuous(limits = c(0, NA))
          })

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x is discrete and y is continuous: boxplot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName",
                        x = "fxDiscreteGeom",
                        y = "fxContinuousGeom"),
          function(fx_name, ..., x, y) {
            function(gg) gg + ggplot2::geom_histogram()
          })

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x is continuous and y is discrete: Stacked density plot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName",
                        x = "fxContinuousGeom",
                        y = "fxDiscreteGeom"),
          function(fx_name, ..., x, y) {
            function(gg) gg +
              ggridges::geom_density_ridges(rel_min_height = .01)
          })

#' @rdname fx_layer_complete_ind
#'
#' * fxGeom, x and y are continuous: scatter plot
#'
#' @export

setMethod("def_fx_layer_complete_ind",
          signature = c(fx_name = "geomFxName",
                        x = "fxContinuousGeom",
                        y = "fxContinuousGeom"),
          function(fx_name, ..., x, y) {
            function(gg) gg + ggplot2::geom_point()
          })
