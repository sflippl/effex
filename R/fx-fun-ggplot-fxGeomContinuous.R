#' @export
#'
#' @rdname fxe_layer_complete_nominate

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "xAesName"),
          function(fx_geom, aes_name, data, ..., name,
                   fxGeom_nominations = NULL,
                   fxGeom_alpha.threshold = NULL, fxGeom_alpha.half = NULL,
                   fxGeom_alpha.min = NULL) {
            nxt <- callNextMethod()
            n_row <- nrow(data)
            alpha <- get_alpha(fxGeom_alpha.threshold, fxGeom_alpha.half,
                               fxGeom_alpha.min, n_row)
            list(
              nomination(ggplot2::geom_point(alpha = alpha)),
              nomination(ggplot2::geom_histogram()),
              nomination(ggplot2::geom_density()),
              nomination(ggplot2::geom_hex(),
                         ggplot2::scale_fill_gradient(
                           limits = c(0, NA),
                           low = "grey80",
                           high = "black"
                         ))
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxe_layer_complete_nominate

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "yAesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL,
                   fxGeom_alpha.threshold = NULL, fxGeom_alpha.half = NULL,
                   fxGeom_alpha.min = NULL) {
            nxt <- callNextMethod()
            n_row <- nrow(data)
            alpha <- get_alpha(fxGeom_alpha.threshold, fxGeom_alpha.half,
                               fxGeom_alpha.min, n_row)
            list(
              nomination(ggplot2::geom_boxplot()),
              nomination(ggplot2::geom_point(alpha = alpha)),
              nomination(ggplot2::geom_hex(),
                         ggplot2::scale_fill_gradient(
                           limits = c(0, NA),
                           low = "grey80",
                           high = "black"
                         ))
            ) %>% c(nxt)
          })

#' @export
#'
#' @rdname fxe_layer_complete_veto

setMethod("fxe_layer_complete_veto",
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
#' @rdname fxe_layer_complete_veto

setMethod("fxe_layer_complete_veto",
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
#' @rdname fxe_layer_complete_vote

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL,
                   fxGeom_alpha.threshold = NULL, fxGeom_alpha.half = NULL,
                   fxGeom_alpha.min = NULL, fxGeom_hex.threshold = NULL) {
            nxt <- callNextMethod()
            n_row <- nrow(data)
            alpha <- get_alpha(fxGeom_alpha.threshold, fxGeom_alpha.half,
                               fxGeom_alpha.min, n_row)
            if(is.null(fxGeom_hex.threshold)) fxGeom_hex.threshold <- 1000
            bool_hex <- n_row >= fxGeom_hex.threshold
            dplyr::case_when(
              any(
                purrr::map_lgl(
                  nom_layers(nomination),
                  ~ inherits(.$geom, "GeomPoint") &
                    identical(.$aes_params$alpha, alpha)
                )
              ) ~ 1,
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$geom, "GeomHex")
                )
              ) ~ bool_hex * 2,
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
#' @rdname fxe_layer_complete_vote

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL,
                   fxGeom_alpha.threshold = NULL, fxGeom_alpha.half = NULL,
                   fxGeom_alpha.min = NULL, fxGeom_hex.threshold = NULL) {
            nxt <- callNextMethod()
            n_row <- nrow(data)
            alpha <- get_alpha(fxGeom_alpha.threshold, fxGeom_alpha.half,
                               fxGeom_alpha.min, n_row)
            if(is.null(fxGeom_hex.threshold)) fxGeom_hex.threshold <- 1000
            bool_hex <- n_row >= fxGeom_hex.threshold
            dplyr::case_when(
              any(
                purrr::map_lgl(
                  nom_layers(nomination),
                  ~ inherits(.$geom, "GeomPoint") &
                    identical(.$aes_params$alpha, alpha)
                )
              ) ~ 1,
              any(
                purrr::map_lgl(
                  nom_layers(nomination), ~ inherits(.$geom, "GeomHex")
                )
              ) ~ bool_hex * 2,
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

get_alpha <- function(fxGeom_alpha.threshold, fxGeom_alpha.half,
                      fxGeom_alpha.min, n_row) {
  if(is.null(fxGeom_alpha.threshold)) fxGeom_alpha.threshold <- 1000
  if(is.null(fxGeom_alpha.half)) fxGeom_alpha.half <- 3000
  if(is.null(fxGeom_alpha.min)) fxGeom_alpha.min <- 0.05
  diff <- fxGeom_alpha.half - fxGeom_alpha.threshold
  decay_rate <- log(0.5) / diff
  if(n_row > fxGeom_alpha.threshold) {
    alpha <- exp(decay_rate * (n_row - fxGeom_alpha.threshold))

    # Make sure that alpha is not zero:
    alpha <- fxGeom_alpha.min + (1 - fxGeom_alpha.min) * alpha
  }
  else alpha <- 1
  alpha
}
