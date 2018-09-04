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
#' @description * fxGeomDiscrete
#'
#' @export

.fxGeomDiscrete <- setClass("fxGeomDiscrete", contains = "fxGeom")

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
  subcls <- names(getClass("fxGeom")@subclasses) %>%
    stringr::str_remove(stringr::coll("fxGeom"))
  if(geom_class %in% subcls)
    return(do.call(paste0(".", "fxGeom", geom_class), list(geom_class)))
  else if(geom_class == "") return(.fxGeom(""))
  else stop(glue::glue("There is no subclass called fxGeom{geom_class}."))
}
