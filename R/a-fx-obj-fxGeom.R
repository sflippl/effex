#' Effex Objects: fxGeom
#'
#' fxGeom is a class that helps with method dispatch. Every subclass of type
#' <subclass> is instantiated by `fxGeom<subclass>` and can be created by
#' calling `fxGeom`. If the corresponding subclass does not exist, the function
#' will throw an error. The available subclasses are:
#'
#' @param geom_class the subclass.
#'
#' @export
#'
#' @examples
#' fxGeom("Continuous")
#' fxGeom("Spatial")
#' fxGeom("")

.fxGeom <- setClass("fxGeom", contains = "character")

#' @rdname fxGeom-class
#'
#' @description * fxGeomContinuous
#'
#' @export

.fxGeomContinuous <- setClass("fxGeomContinuous", contains = "fxGeom")

#' @rdname fxGeom-class
#'
#' @description
#'     + fxGeomContinuousCI
#'
#' @export

.fxGeomContinuousCI <- setClass("fxGeomContinuousCI",
                                contains = c("fxGeomContinuous"))

#' @rdname fxGeom-class
#'
#' @description
#'     + fxGeomTime
#'
#' @export

.fxGeomTime <- setClass("fxGeomTime", contains = "fxGeomContinuous")

#' @rdname fxGeom-class
#'
#' @description
#' * fxGeomCI
#'
#' @export

#' @rdname fxGeom-class
#'
#' @description * fxGeomDiscrete
#'
#' @export

.fxGeomDiscrete <- setClass("fxGeomDiscrete", contains = "fxGeom")

#' @rdname fxGeom-class
#'
#' @description
#'     + fxGeomOrdinal
#'
#' @export

.fxGeomOrdinal <- setClass("fxGeomOrdinal", contains = "fxGeomDiscrete")

#' @rdname fxGeom-class
#'
#' @description
#'       + fxGeomOrdinalCI
#'
#' @export

.fxGeomOrdinalCI <- setClass("fxGeomOrdinalCI",
                             contains = c("fxGeomOrdinal"))

#' @rdname fxGeom-class
#'
#' @description * fxGeomSpatial
#'
#' @export

.fxGeomSpatial <- setClass("fxGeomSpatial", contains = "fxGeom")

#' @rdname fxGeom-class
#'
#' @export

fxGeom <- function(geom_class = "") {
  do.call(paste0(".", "fxGeom", geom_class), list(geom_class))
  # subcls <- names(getClass("fxGeom")@subclasses) %>%
  #   stringr::str_remove(stringr::coll("fxGeom"))
  # if(geom_class %in% subcls)
  #   return(do.call(paste0(".", "fxGeom", geom_class), list(geom_class)))
  # else if(geom_class == "") return(.fxGeom(""))
  # else stop(glue::glue("There is no subclass called fxGeom{geom_class}."))
}
