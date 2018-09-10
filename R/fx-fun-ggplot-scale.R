#' Effex External: Scale layers
#'
#' This function provides an extendible scheme for scales for a single layer.
#' If some features are to be added, it is often sensible to `callNextMethod()`
#' and then add these features.
#'
#' @inheritParams fxe_layer_single
#'
#' @section Methods:
#' Implemented methods are:
#'
#' @export

setGeneric("fxe_layer_scale",
           function(fx_geom, aes_name, ...)
             standardGeneric("fxe_layer_scale"))

#' @rdname fxe_layer_scale
#'
#' @param fxGeom_title `NULL` or a character that provides the title
#'
#' @section Methods:
#' * `fx_geom = fxGeom(), aes_name = AesName()`: Adds labels and limits
#'
#' @export

setMethod(
  "fxe_layer_scale",
  signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
  function(fx_geom, aes_name, data,
           fxGeom_title = NULL, name = "", ...) {
    if(is.null(fxGeom_title))
      title <- fx_info(data, "title") %>% dplyr::filter(name == !!name) %>%
        magrittr::extract2(" ")
    else if(identical(fxGeom_title, list(NULL))) title <- NULL
    else title <- fxGeom_title
    assertthat::assert_that(aes_name != "")
    list(
      ggplot2::labs(rlang::list2(!!aes_name := title))
    )
  }
)

#' @rdname fxe_layer_scale
#'
#' @section Methods:
#' * `fx_geom = fxGeom("Continuous"), aes_name = AesName("")`
#'
#' @export

setMethod(
  "fxe_layer_scale",
  signature = c(fx_geom = "fxGeomContinuous", aes_name = "AesName"),
  function(fx_geom, aes_name, data,
           fxGeom_breaks = NULL, fxGeom_minor_breaks = NULL,
           fxGeom_labels = NULL,
           fxGeom_expand.add = NULL, fxGeom_expand.mult = NULL,
           fxGeom_na.value = NULL, fxGeom_trans = "identity",
           fxGeom_limits = NULL,
           fxGeom_pal.x_y = NULL, fxGeom_pal.alpha = NULL,
           fxGeom_pal.colour_fill = NULL, fxGeom_pal.size = NULL, ...) {
    nxt <- callNextMethod()
    # First, we set the relatively obvious parameters:
    breaks <- null_helper(fxGeom_breaks, ggplot2::waiver())
    minor_breaks <- null_helper(fxGeom_breaks, ggplot2::waiver())
    labels <- null_helper(fxGeom_labels, ggplot2::waiver())
    if(is.null(fxGeom_expand.add)) fxGeom_expand.add <- 0
    if(is.null(fxGeom_expand.mult)) fxGeom_expand.mult <- .05
    na.value <- if(is.null(fxGeom_na.value)) NA_real_ else fxGeom_na.value
    trans <- fxGeom_trans
    # Now we set the palette
    fxGeom_palette <- switch(
      aes_name,
      x = fxGeom_pal.x_y,
      y = fxGeom_pal.x_y, alpha = fxGeom_pal.alpha,
      colour = fxGeom_pal.colour_fill, color = fxGeom_pal.colour_fill,
      fill = fxGeom_pal.colour_fill, size = fxGeom_pal.size,
      stop("Aesthetic ", aes_name, " not supported by the general scale.")
    )
    if(is.null(fxGeom_palette))
      fxGeom_palette <- fxe_default_fxGeom_pal(fx_geom, aes_name)
    sc <- do.call(
      glue::glue("scale_{aes_name}_continuous"),
      list(
        breaks = breaks, minor_breaks = minor_breaks, labels = labels,
        limits = fxGeom_limits,
        expand = ggplot2::expand_scale(add = fxGeom_expand.add,
                                       mult = fxGeom_expand.mult),
        na.value = na.value,
        trans = fxGeom_trans
      ),
      envir = asNamespace("ggplot2")
    )
    sc$palette <- fxGeom_palette
    sc %>%
      list() %>% c(nxt, .)
  }
)

#' @rdname fxe_layer_scale
#'
#' @section Methods:
#' * `fx_geom = fxGeom("Discrete"), aes_name = AesName("")`
#'
#' @export

setMethod(
  "fxe_layer_scale",
  signature = c(fx_geom = "fxGeomDiscrete", aes_name = "AesName"),
  function(fx_geom, aes_name, data,
           fxGeom_breaks = NULL, fxGeom_limits = NULL, fxGeom_drop = NULL,
           fxGeom_na.translate = NULL, fxGeom_labels = NULL,
           fxGeom_expand.add = NULL, fxGeom_expand.mult = NULL,
           fxGeom_pal.x_y = NULL,
           fxGeom_pal.colour_fill = NULL, fxGeom_pal.shape = NULL,
           ...) {
    nxt <- callNextMethod()
    breaks <- null_helper(fxGeom_breaks, ggplot2::waiver())
    if(is.null(fxGeom_drop)) fxGeom_drop <- FALSE
    if(is.null(fxGeom_na.translate)) fxGeom_na.translate <- TRUE
    if(is.null(fxGeom_expand.add)) fxGeom_expand.add <- 0.6
    if(is.null(fxGeom_expand.mult)) fxGeom_expand.mult <- 0
    labels <- null_helper(fxGeom_labels, ggplot2::waiver())
    assertthat::assert_that(aes_name != "")
    # Now we set the palette
    fxGeom_palette <- switch(
      aes_name,
      x = fxGeom_pal.x_y,
      y = fxGeom_pal.x_y,
      colour = fxGeom_pal.colour_fill, color = fxGeom_pal.colour_fill,
      fill = fxGeom_pal.colour_fill, shape = fxGeom_pal.shape,
      stop("Aesthetic ", aes_name, " not supported by the general scale.")
    )
    if(is.null(fxGeom_palette))
      fxGeom_palette <- fxe_default_fxGeom_pal(fx_geom, aes_name)
    sc <- do.call(
      glue::glue("scale_{aes_name}_discrete"),
      list(
        breaks = breaks, labels = labels,
        limits = fxGeom_limits,
        expand = ggplot2::expand_scale(add = fxGeom_expand.add,
                                       mult = fxGeom_expand.mult),
        na.translate = fxGeom_na.translate, drop = fxGeom_drop
      ),
      envir = asNamespace("ggplot2")
    )
    sc$palette <- fxGeom_palette
    sc %>%
      list() %>% c(nxt, .)
  }
)
