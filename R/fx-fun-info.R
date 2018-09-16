#' Effex Function: Information
#'
#' This function provides versatile information on the different variables.
#' [fx_output()] turns this information into a string.
#'
#' @param data A dataframe, possibly with a metaframe
#' @param topic The topic of the information
#' @param arguments for the information methods
#'
#' @section Extension Mechanism:
#' An extension may be defined via `fxe_info()` which dispatches over
#' `fxd("info", topic)`.
#'
#' @export

fx_info <- function(data, topic, ...) {
  data <- data %>%
    fx_default(columns = fx_info_columns) %>%
    fx_evaluate()
  assertthat::assert_that(is.character(topic))
  lst <- purrr::map(topic, function(top) fxe_info(data, fxd("info", top), ...))
  purrr::reduce(lst, ~ dplyr::inner_join(.x, .y, by = "name"))
}

#' @rdname fx_info

fxe_info <- function(data, topic, ...) UseMethod("fxe_info", topic)

#' @describeIn fx_info If no method for this topic is supplied, this function
#' looks up whether a column `fxInfo_<column>` exists.
#'
#' @export

fxe_info.fxd_info <- function(data, topic, ...) {
  colname <- paste0("fxInfo_", fxd_subclass(topic))
  if(colname %in% names(metaframe(data))) {
    coltitle <-
      stringr::str_to_title(stringr::str_replace_all(fxd_subclass(topic),
                                                     stringr::coll("_"), " "))
    mf <- metaframe(data)
    mf[[coltitle]] <- mf[[colname]]
    return(dplyr::select(mf, name, !!coltitle))
  }
  stop("No method for topic ", fxd_subclass(topic), "provided")
}
