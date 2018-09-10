#' AesName
#'
#' AesName is a very small class that allows for flexible inheritance of
#' `fxext_<>`-functions (e. g. [fx_ext_layer_single()]). Whereas any name may be
#' used, the available subclasses are:
#'
#' @details If other aesthetics are supplied, most methods default to a neutral
#' element, e. g. `identity` in the case of [fx_ggplot()]. You can build
#' `AesName` by calling the function and you can set its subclasses by
#' `new("<subclass>AesName", ...)`. This is, however, not recommended. It is
#' better to use the `aes` argument in [AesName()], as it defaults to generating
#' `AesName` if the `aes` is unknown.
#'
#' @export

.AesName <- setClass("AesName", contains = "character")

#' @rdname AesName-class
#'
#' @description * x
#'
#' @export

.xAesName <- setClass("xAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * y
#'
#' @export

.yAesName <- setClass("yAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * alpha
#'
#' @export

.alphaAesName <- setClass("alphaAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * colour
#'
#' @export

.colourAesName <- setClass("colourAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * fill
#'
#' @export

.fillAesName <- setClass("fillAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * group
#'
#' @export

.groupAesName <- setClass("groupAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * shape
#'
#' @export

.shapeAesName <- setClass("shapeAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * size
#'
#' @export

.sizeAesName <- setClass("sizeAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * stroke
#'
#' @export

.strokeAesName <- setClass("strokeAesName", contains = "AesName")

#' @rdname AesName-class
#'
#' @description * geom
#'
#' @export

.geomAesName <- setClass("geomAesName", contains = "AesName")

#' @param aes The aes. If a subclass "<aes>AesName" exists, this will be
#' instantiated.
#'
#' @export
#'
#' @examples
#' AesName(aes = "x")
#' AesName(aes = "m")
#' AesName(aes = "colour")
#'
#' @rdname AesName-class

AesName <- function(aes) {
  subcls <- names(getClass("AesName")@subclasses) %>%
    stringr::str_remove(stringr::coll("AesName"))
  if(aes %in% subcls)
    return(do.call(paste0(".", aes, "AesName"), list(aes)))
  else return(.AesName(aes))
}

setClassUnion("x_yAesName", members = c("xAesName", "yAesName"))

setClassUnion("colour_fillAesName", members = c("colourAesName", "fillAesName"))
