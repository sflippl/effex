#' Effex Functions: Default of the column `fxGeom_class`
#'
#' `fxGeom_class` specifies the class of the `fxGeom` object which [fx_ggplot()]
#' works on. `fx_default_fxGeom_class` infers the class according to the class of
#' the corresponding column.
#'
#' @param data a data frame
#' @param mf a metaframe
#'
#' @section Mechanism:
#' * Numeric: [fxGeomContinuous-class](fxGeomContinuous)
#' * Character or Factor: [fxGeomDiscrete-class](fxGeomDiscrete)
#' * Simple Feature column: [fxGeomSpatial-class](fxGeomSpatial)
#' * Else: [fxGeom-class](fxGeom)
#'
#' @export

fx_default_fxGeom_class <- function(data, mf = metaframe(fx_default(data))) {
  fxGeom_classes <- purrr::map_chr(
    mf$name,
    ~ dplyr::case_when(
      is.numeric(data[[.]]) ~ "Continuous",
      is.factor(data[[.]]) | is.character(data[[.]]) ~ "Discrete",
      inherits(data[[.]], "sfc") ~ "Spatial",
      TRUE ~ ""
    )
  )
  fxGeom_classes
}

#' @export
#'
#' @rdname fxe_default

fxe_default.fxd_default_fxGeom_class <- function(data, mf, col, ...)
  fx_default_fxGeom_class(data, mf)

#' Effex Functions: Default of `fxGeom_limits`
#'
#' `fxGeom_limits` specifies the limits of the scale of [fx_ggplot()].
#'
#' @section Mechanism:
#' * Numeric: The range of the data
#' * Character: The unique values
#' * Factor: The levels
#' * Else: NULL
#'
#' @export

fx_default_fxGeom_limits <- function(data, mf = metaframe(fx_default(data))) {
  fxGeom_limits <- purrr::map(
    mf$name,
    function(nam) {
      if(is.numeric(data[[nam]])) return(range(data[[nam]], na.rm = TRUE))
      else if(is.character(data[[nam]])) {
        un <- unique(data[[nam]])
        return(un[!is.na(un)])
      }
      else if(is.factor(data[[nam]])) return(levels(data[[nam]]))
      NULL
    }
  )
  fxGeom_limits
}

#' @export
#'
#' @rdname fxe_default

fxe_default.fxd_default_fxGeom_limits <- function(data, mf, col, ...)
  fx_default_fxGeom_limits(data, mf)

#' Effex Functions: Default of `fxGeom_trans`
#'
#' The default transformation is determined in a manner that is sensitive to
#' skewness. Skewed distribution are often hard to interpret if the
#' axes are not transformed.
#'
#' @param fxGeom_trans_simple `logical` should all boxcox and modulus
#' transformations be allowed or should they be restricted to "identity",
#' "sqrt" and "log10"?
#' @param fxGeom_trans_p.threshold a value between 0 and 1. How low does the
#' p-value of the [moments::agostino.test()] have to be to make another
#' transformation necessary? A rather low value is recommended.
#'
#' @section Mechanism:
#' This function has two different modi: `fxGeom_trans_simple = TRUE/FALSE`.
#' `TRUE` means that the only possible transformations are "identity", "sqrt"
#' and "log10". `FALSE`, on the other hand, allows for the entire range of
#' boxcox transformations.
#'
#' Non-numeric data is always assigned "identity".
#'
#' If `fxGeom_trans_simple` is true, the skewness of the three transformation is
#' compared and the lowest absolute skewness wins. If there is nonpositive
#' numeric data, "identity" is chosen by default.
#'
#' If `fxGeom_trans_simple` is false, the procedure consists of severals steps:
#'
#' * To avoid unnecessary transformations, the [moments::agostino.test()] for
#' unnormal skewness is conducted. If the corresponding p-value is not below
#' `fxGeom_trans_p.threshold`, the transformation is "identity".
#' * Afterwards, [geoR::boxcoxfit()] determines a suitable transformation
#' which is then transformed into a suitable object by [scales::boxcox_trans()]
#'
#'
#' @seealso [scales::boxcox_trans()]
#'
#' @examples
#' set.seed(1)
#' norm <- rnorm(1000)
#' square <- norm^2
#' exp <- exp(norm)
#' df <- dplyr::tibble(
#'   norm = rnorm(100),
#'   square = rnorm(100)^2,
#'   exp = exp(rnorm(100)),
#'   chars = rep("a", 100)
#' )
#' fx_default_fxGeom_trans(df)
#' fx_default_fxGeom_trans(fxGeom_trans_simple = FALSE)
#'
#' @export

fx_default_fxGeom_trans <- function(
  data, mf = metaframe(fx_default(data)),
  fxGeom_trans_simple = TRUE, fxGeom_trans_p.threshold = 0.01
) {
  ret <- purrr::map_chr(
    seq_len(nrow(mf)),
    function(i) {
      nam <- mf$name[i]
      if(!is.numeric(data[[nam]]) | any(data[[nam]] <= 0, na.rm = TRUE))
        return("identity")
      ident <- data[[nam]]
      sqrt <- sqrt(ident)
      log <- log10(ident)
      winner <- which.min(abs(c(
        moments::skewness(ident, na.rm = TRUE),
        moments::skewness(sqrt, na.rm = TRUE),
        moments::skewness(log, na.rm = TRUE)
      )))
      return(c("identity", "sqrt", "log10")[winner])
    }
  )
  if(!fxGeom_trans_simple) {
    ret <- purrr::map(
      seq_len(nrow(mf)),
      function(i) {
        nam <- mf$name[i]
        if(!is.numeric(data[[nam]])) return("identity")
        x <- data[[nam]] %>% na.omit()
        # The test can only work with a maximal n of 46340. Random sampling should
        # take care of it.
        if(length(x) > 46340)
          x <- sample(x, size = 46340)
        test <- moments::agostino.test(x)
        if(test$p.value >= fxGeom_trans_p.threshold)
          return("identity")
        # boxcoxfit sometimes fails. In this case, we use the simple version.
        bc <- tryCatch(
          geoR::boxcoxfit(data[[nam]], lambda2 = TRUE),
          error = function(e) NULL
        )
        if(is.null(bc)) return(ret[[i]])
        lambda <- bc$lambda[1]
        offset <- bc$lambda[2]
        scales::boxcox_trans(p = lambda, offset = offset)
      }
    )
  }
  ret
}

#' @export
#'
#' @rdname fxe_default

fxe_default.fxd_default_fxGeom_trans <- function(
  data, mf, col, ...) {
  dots <- rlang::list2(...)
  dots <- dots[names(dots) %in%
                 c("fxGeom_trans_simple", "fxGeom_trans_p.threshold")]
  args <- rlang::list2(
    data = data, mf = mf, !!!dots
  )
  do.call(fx_default_fxGeom_trans, args)
}

#' Effex Functions: Default of `fxGeom_pal`
#'
#' The default palette depends on the aesthetic that is used and mostly
#' corresponds to the default choices in [ggplot2]. It is extendible via
#' `fxe_default_fxGeom_pal` over `fx_geom` and `aes_name`.
#'
#' @export

fx_default_fxGeom_pal <-
  function(data, mf = metaframe(fx_default(data)), aes_name) {
  metaframe(data) <- mf
  data <- fx_default(data, columns = "fxGeom_class")
  purrr::map(
    metaframe(data)$fxGeom_class,
    function(cls) fxe_default_fxGeom_pal(fxGeom(cls), AesName(aes_name))
  )
  }

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setGeneric("fxe_default_fxGeom_pal",
           function(fx_geom, aes_name, ...)
             standardGeneric("fxe_default_fxGeom_pal"))

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomContinuous", aes_name = "x_yAesName"),
          function(fx_geom, aes_name, ...) identity)

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomContinuous",
                        aes_name = "colour_fillAesName"),
          function(fx_geom, aes_name, ...)
            scales::seq_gradient_pal("#132B43", "#56B1F7", "Lab"))

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomContinuous",
                        aes_name = "alphaAesName"),
          function(fx_geom, aes_name, ...)
            scales::rescale_pal())

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomContinuous",
                        aes_name = "sizeAesName"),
          function(fx_geom, aes_name, ...)
            scales::area_pal())

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomDiscrete", aes_name = "x_yAesName"),
          function(fx_geom, aes_name, ...) identity)

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomDiscrete",
                        aes_name = "colour_fillAesName"),
          function(fx_geom, aes_name, ...) scales::hue_pal())

#' @rdname fx_default_fxGeom_pal
#'
#' @export

setMethod("fxe_default_fxGeom_pal",
          signature = c(fx_geom = "fxGeomDiscrete",
                        aes_name = "shapeAesName"),
          function(fx_geom, aes_name, ...)
            scales::shape_pal())
