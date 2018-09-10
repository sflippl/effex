#' Effex Externals: Other single layer arguments
#'
#' This external function is used to add single layer arguments that are not
#' related to the scale. Note that there is no strict restriction for actual
#' `Scale`-arguments in [fxe_layer_scale()] and e. g. slight theme tweaks
#' might make sense sometimes.
#'
#' @export

setGeneric("fxe_layer_other",
           function(fx_geom, aes_name, ...)
             standardGeneric("fxe_layer_other"))

#' @rdname fxe_layer_other
#'
#' @export

setMethod("fxe_layer_other",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(fx_geom, aes_name, ...) list())
