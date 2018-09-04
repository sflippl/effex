#' fxName class
#'
#' In order to allow for flexible inheritance of additive functions, we define
#' a dummy class for every fx name (see also [aesName()]). The available methods
#' are:
#'
#' @seealso aesName
#'
#' @rdname fxName
#'
#' @export

.fxName <- setClass("fxName", slots = c(fx = "character"))

#' @rdname fxName
#'
#' @description * indicators
#'
#' @export

.indicatorsFxName <- setClass("indicatorsFxName", contains = "fxName")

#' @rdname fxName
#'
#' @description * geom
#'
#' @export

.geomFxName <- setClass("geomFxName", contains = "fxName")

#' @rdname fxName
#'
#' @description * description
#'
#' @export

.descriptionFxName <- setClass("descriptionFxName", contains = "fxName")

#' @param fx The fx If a subclass "<fx>FxName" exists, this will be
#' instantiated.
#'
#' @export
#'
#' @examples
#' fxName(fx = "geom")
#' fxName(fx = "description")
#' fxName(fx = "x")

fxName <- function(fx) {
  subcls <- names(getClass("fxName")@subclasses) %>%
    stringr::str_remove(stringr::coll("FxName"))
  if(fx %in% subcls)
    return(do.call(paste0(".", fx, "FxName"), list(fx = fx)))
  else return(.fxName(fx = fx))
}
