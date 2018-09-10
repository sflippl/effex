#' @export
#'
#' @rdname fxe_layer_complete_nominate

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            nxt <- callNextMethod()
            list(
              nomination(ggplot2::geom_bar()),
              nomination(ggplot2::geom_bin2d()),
              nomination(ggplot2::geom_boxplot())
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxe_layer_complete_nominate

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            nxt <- callNextMethod()
            list(
              nomination(ggplot2::geom_bin2d())
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxe_layer_complete_veto

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL) {
            nxt <- callNextMethod()
            ret <- any(
              purrr::map_lgl(nom_layers(nomination),
                             ~ inherits(.$stat, "StatDensity"))
            )
            ret | nxt
          })

#' @export
#'
#' @rdname fxe_layer_complete_veto

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL) {
            nxt <- callNextMethod()
            ret <- any(
              purrr::map_lgl(nom_layers(nomination),
                             ~ (inherits(.$stat, "StatBin") &
                                  inherits(.$geom, "GeomBar")) |
                               (inherits(.$stat, "StatDensity")) |
                               inherits(.$stat, "StatCount") |
                               inherits(.$geom, "GeomBoxplot"))
            )
            ret | nxt
          })

#' @export
#'
#' @rdname fxe_layer_complete_vote

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL) {
            nxt <- callNextMethod()
            dplyr::case_when(
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$geom, "StatBin2d"))) ~
                1,
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$geom, "GeomBar") &
                                   inherits(.$stat, "StatCount"))) ~ 2,
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$geom, "GeomBoxplot"))) ~ 1,
              TRUE ~ 0
            ) + nxt
          })

#' @export
#'
#' @rdname fxe_layer_complete_vote

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL) {
            nxt <- callNextMethod()
            dplyr::case_when(
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$geom, "StatBin2d"))) ~
                1,
              TRUE ~ 0
            ) + nxt
          })
