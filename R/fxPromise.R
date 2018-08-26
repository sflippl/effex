#' Effex-Promises
#'
#' This class describes how to generate a certain fx object from
#' the indicator dataframe. The function `createFx` then yields the `fx` object.
#'
#' @export

setClass("fxp", contains = "VIRTUAL")

#' @rdname fxp-class
#'
#' @export

setGeneric("createFx", function(fxp, indicator) standardGeneric("createFx"))

#' @export

setOldClass("uneval")
setOldClass(c("tbl_df", "tbl", "data.frame"))
