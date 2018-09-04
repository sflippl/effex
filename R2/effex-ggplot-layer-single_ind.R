#' @include effex-ggplot.R effex-ggplot-s4-fxName.R effex-ggplot-s4-aesName.R
#' fxDescription.R fxIndname.R fx.R fxGeom.R
NULL

#' @rdname def_fx_layer_single_ind
#'
#' * Default: returns identity
#'
#' @export

setMethod("def_fx_layer_single_ind",
          signature = c(aes_name = "aesName", fx_name = "fxName", fx = "fx"),
          function(aes_name, fx_name, fx, indicator) return(identity))

#' @rdname def_fx_layer_single_ind
#'
#' * descriptions: Provides a [fxDescription()](description). `label` is used
#' as the label of the scale corresponding to `aes_name`.
#'
#' @export

setMethod("def_fx_layer_single_ind",
          signature = c(aes_name = "aesName",
                        fx_name = "descriptionFxName",
                        fx = "fxDescription"),
          function(aes_name, fx_name, fx) {
            label <- fx@tbl[["label"]]
            if(is.null(label)) return(identity)
            lab_list <- list(label)
            names(lab_list) <- aes_name@aes
            function(gg) {
              gg + ggplot2::labs(rlang::splice(lab_list))
            }
          })

#' @rdname def_fx_layer_single_ind
#'
#' * geom: Sets the ranges.
#'
#' @export

setMethod("def_fx_layer_single_ind",
          signature = c(aes_name = "aesName",
                        fx_name = "geomFxName",
                        fx = "fxContinuousGeom"),
          function(aes_name, fx_name, fx) {
            function(gg) {
              fun <- get(paste0("scale_", aes_name@aes, "_continuous"),
                         asNamespace("ggplot2"))
              scale <- do.call(fun, list(limits = fx@limits))
              gg + scale
            }
          })

#' @rdname def_fx_layer_single_ind
#'
#' @export

setMethod("def_fx_layer_single_ind",
          signature = c(aes_name = "aesName",
                        fx_name = "geomFxName",
                        fx = "fxDiscreteGeom"),
          function(aes_name, fx_name, fx) {
            function(gg) {
              fun <- get(paste0("scale_", aes_name@aes, "_discrete"),
                         asNamespace("ggplot2"))
              scale <- do.call(fun, list(limits = fx@limits))
              gg + scale
            }
          })
