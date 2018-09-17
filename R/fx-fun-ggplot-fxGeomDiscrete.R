#' @export
#'
#' @describeIn fxe_layer_complete_nominate
#'     + bar plot
#'     + heatmap
#'     + boxplot

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_nominate(
              fxGeom("Discrete"), AesName(""), data, ...
            )
            list(
              nomination(ggplot2::geom_bar()),
              nomination(ggplot2::geom_bin2d()),
              nomination(ggplot2::geom_boxplot())
            ) %>% c(nxt)
          })

#' @export
#'
#' @describeIn fxe_layer_complete_nominate
#'     + heatmap

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_nominate(
              fxGeom("Discrete"), AesName(""), data, ...
            )
            list(
              nomination(ggplot2::geom_bin2d())
            ) %>% c(nxt)
          })

#' @export
#'
#' @describeIn fxe_layer_complete_veto
#'     + density plots

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_veto(
              nomination, fxGeom("Discrete"), AesName(""), data, ...
            )
            ret <- any(
              purrr::map_lgl(nom_layers(nomination),
                             ~ inherits(.$stat, "StatDensity"))
            )
            ret | nxt
          })

#' @export
#'
#' @rdname fxe_layer_complete_veto
#'     + histograms
#'     + density plots
#'     + bar plots
#'     + boxplots

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_veto(
              nomination, fxGeom("Discrete"), AesName(""), data, ...
            )
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
#' @describeIn fxe_layer_complete_vote
#'     + heatmap: 1
#'     + barplot: 2
#'     + boxplot: 1

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_vote(
              nomination, fxGeom("Discrete"), AesName(""), data, ...
            )
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
#' @describeIn fxe_layer_complete_vote
#'     + heatmap: 1

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_vote(
              nomination, fxGeom("Discrete"), AesName(""), data, ...
            )
            dplyr::case_when(
              any(purrr::map_lgl(nom_layers(nomination),
                                 ~ inherits(.$geom, "StatBin2d"))) ~
                1,
              TRUE ~ 0
            ) + nxt
          })
