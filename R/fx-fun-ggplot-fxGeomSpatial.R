#' @export
#'
#' @describeIn fxe_layer_single produces an error as the aesthetic must be
#' `geom`

setMethod(
  "fxe_layer_single",
  signature = c(fx_geom = "fxGeomSpatial", aes_name = "AesName"),
  function(fx_geom, aes_name, data, ...) {
    stop("Spatial variables can only be processed by the aesthetic geom, not ",
         "by ", aes_name, ".")
  }
)

#' @export
#'
#' @param fxGeom_xlim see [ggplot2::coord_sf()]
#' @param fxGeom_ylim see [ggplot2::coord_sf()]
#' @param fxGeom_expand see [ggplot2::coord_sf()]
#' @param fxGeom_crs see [ggplot2::coord_sf()]
#'
#' @describeIn fxe_layer_single

setMethod(
  "fxe_layer_single",
  signature = c(fx_geom = "fxGeomSpatial", aes_name = "geomAesName"),
  function(fx_geom, aes_name, data,
           fxGeom_xlim = NULL, fxGeom_ylim = NULL, fxGeom_expand = NULL,
           fxGeom_crs = NULL, ...) {
    if(is.null(fxGeom_expand)) fxGeom_expand <- TRUE
    list(
      ggplot2::coord_sf(xlim = fxGeom_xlim, ylim = fxGeom_ylim,
                        expand = fxGeom_expand,
                        crs = fxGeom_crs)
    )
  }
)

#' @export
#'
#' @describeIn fxe_layer_complete_nominate

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomSpatial", aes_name = "geomAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            nxt <- callNextMethod()
            list(nomination(ggplot2::geom_sf())) %>%
              c(nxt)
          })

#' @export
#'
#' @describeIn fxe_layer_complete_veto vetoes anything that does not inherit
#' from [ggplot2::GeomSf]

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeomSpatial", aes_name = "geomAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL) {
            nxt <- callNextMethod()
            ret <- !any(
              purrr::map_lgl(
                nom_layers(nomination),
                ~ inherits(.$geom, "GeomSf")
              )
            )
            ret | nxt
          })
