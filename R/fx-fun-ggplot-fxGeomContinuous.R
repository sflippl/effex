#' @export
#'
#' @rdname fxext_layer_complete_nominate

setMethod("fxext_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "xAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            nxt <- callNextMethod()
            list(
              nomination(ggplot2::geom_point()),
              nomination(ggplot2::geom_histogram()),
              nomination(ggplot2::geom_density())
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxext_layer_complete_nominate

setMethod("fxext_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "yAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            nxt <- callNextMethod()
            list(
              nomination(ggplot2::geom_boxplot()),
              nomination(ggplot2::geom_point())
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxext_layer_complete_veto

setMethod("fxext_layer_complete_veto",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL) {
            nxt <- callNextMethod()
            ret <- any(
              purrr::map_lgl(nom_layers(nomination),
                             ~ inherits(.$geom, "GeomBoxplot") |
                               inherits(.$stat, "StatBoxplot") |
                               inherits(.$stat, "StatCount"))
            )
            ret | nxt
          })

#' @export
#'
#' @rdname fxext_layer_complete_veto

setMethod("fxext_layer_complete_veto",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL) {
            nxt <- callNextMethod()
            ret <- any(
              purrr::map_lgl(nom_layers(nomination),
                             ~ inherits(.$stat, "StatCount") |
                               (inherits(.$stat, "StatBin") &
                                  inherits(.$geom, "GeomBar")) |
                               (inherits(.$stat, "StatDensity")))
            )
            ret | nxt
          })

#' @export
#'
#' @rdname fxext_layer_complete_vote

setMethod("fxext_layer_complete_vote",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL) {
            nxt <- callNextMethod()
            dplyr::case_when(
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$geom, "GeomPoint")
                )
              ) ~ 1,
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$geom, "GeomBar") &
                                   inherits(.$stat, "StatBin")
                )
              ) ~ 2,
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$stat, "StatBin"))) ~ 1,
              TRUE ~ 0
            ) + nxt
          })

#' @export
#'
#' @rdname fxext_layer_complete_vote

setMethod("fxext_layer_complete_vote",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL) {
            nxt <- callNextMethod()
            dplyr::case_when(
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$geom, "GeomPoint")
                )
              ) ~ 1,
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$stat, "StatBoxplot")
                )
              ) ~ 2,
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$stat, "StatBin")
                )
              ) ~ 1,
              TRUE ~ 0
            ) + nxt
          })
