#' aesName class
#'
#' In order to allow flexible inheritance, we define a dummy class for every
#' name of an aesthetic. Whereas any name may be used, the available methods
#' are:
#'
#' @details If other aesthetics are supplied, most methods default to a neutral
#' element, e. g. `identity` in the case of [fx_ggplot()]. You can build
#' `aesName` by calling the function and you can set its subclasses by
#' `new("<subclass>AesName", ...)`. This is, however, not recommended. It is
#' better to use the `sub` argument in [aesName()], as it defaults to generating
#' `aesName` if the `sub` is unknown.
#'
#' @rdname aesName
#'
#' @export

.aesName <- setClass("aesName", slots = c(aes = "character"))

#' @rdname aesName
#'
#' @description * x
#'
#' @export

.xAesName <- setClass("xAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * y
#'
#' @export

.yAesName <- setClass("yAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * alpha
#'
#' @export

.alphaAesName <- setClass("alphaAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * colour
#'
#' @export

.colourAesName <- setClass("colourAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * fill
#'
#' @export

.fillAesName <- setClass("fillAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * group
#'
#' @export

.groupAesName <- setClass("groupAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * shape
#'
#' @export

.shapeAesName <- setClass("shapeAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * size
#'
#' @export

.sizeAesName <- setClass("sizeAesName", contains = "aesName")

#' @rdname aesName
#'
#' @description * stroke
#'
#' @export

.strokeAesName <- setClass("strokeAesName", contains = "aesName")

#' @param aes The aes. If a subclass "<aes>AesName" exists, this will be
#' instantiated.
#'
#' @export
#'
#' @examples
#' aesName(aes = "x")
#' aesName(aes = "m")
#' aesName(aes = "colour")

aesName <- function(aes) {
  subcls <- names(getClass("aesName")@subclasses) %>%
    stringr::str_remove(stringr::coll("AesName"))
  if(aes %in% subcls)
    return(do.call(paste0(".", aes, "AesName"), list(aes = aes)))
  else return(.aesName(aes = aes))
}
