#' Effex External: Scale layers
#'
#' This function provides an extendible scheme for scales for a single layer.
#' If some features are to be added, it is often sensible to `callNextMethod()`
#' and then add these features.
#'
#' @inheritParams fxext_layer_single
#'
#' @section Methods:
#' Implemented methods are:
#'
#' @export

setGeneric("fxext_layer_scale",
           function(fx_geom, aes_name, ...)
             standardGeneric("fxext_layer_scale"))

#' @rdname fxext_layer_scale
#'
#' @param fxGeom_title `NULL` or a character that provides the title
#'
#' @section Methods:
#' * `fx_geom = fxGeom(), aes_name = AesName()`: Adds labels and limits
#'
#' @export

setMethod(
  "fxext_layer_scale",
  signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
  function(fx_geom, aes_name, data,
           fxGeom_title = NULL, fxGeom_limits = c(NA_real_, NA_real_), ...) {
    if(is.null(fxGeom_title)) title <- fx_info(data, "title")[[" "]]
    else if(identical(fxGeom_title, list(NULL))) title <- NULL
    else title <- fxGeom_title
    assertthat::assert_that(aes_name != "")
    list(
      ggplot2::labs(rlang::list2(!!aes_name := title)),
      do.call("lims", rlang::list2(!!aes_name := fxGeom_limits),
              envir = asNamespace("ggplot2"))
    )
  }
)

#' @rdname fxext_layer_scale
#'
#' @section Methods:
#' * `fx_geom = fxGeom("Continuous"), aes_name = AesName("")`:
#'
#' @export

setMethod(
  "fxext_layer_scale",
  signature = c(fx_geom = "fxGeomContinuous", aes_name = "AesName"),
  function(fx_geom, aes_name, fxGeom_breaks = NULL, fxGeom_minor_breaks = NULL,
           fxGeom_labels = NULL, fxGeom_expand = NULL,
           fxGeom_na.value = NULL, fxGeom_trans = "identity", ...) {
    breaks <- null_helper(fxGeom_breaks, waiver())
    minor_breaks <- null_helper(fxGeom_breaks, waiver())
    labels <- null_helper(fxGeom_labels, waiver())
    expand <- null_helper(fxGeom_expand, waiver())
    na.value <- if(is.null(fxGeom_na.value)) NA_real_ else fxGeom_na.value
    trans <- fxGeom_trans
    assertthat::assert_that(aes_name != "")
    do.call(glue::glue("scale_{aes_name}_continuous"),
            list(breaks = breaks, minor_breaks = minor_breaks, labels = labels,
                 expand = expand,
                 na.value = na.value, trans = trans),
            envir = asNamespace("ggplot2")) %>%
      list()
  }
)
