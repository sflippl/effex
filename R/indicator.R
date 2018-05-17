#' Indicator
#'
#' Indicators are promises of data. They consist of a source and a name that,
#' together should be unique. The promised data may depend on further attributes
#' such as the file they may be found in, some kind of selection criterion etc.
#'
#' @section Workflow:
#' If functions like [create()] encounter a non-indicator, they try to turn it
#' to an indicator with `as_indicator()`. This allows a certain flexibility in
#' the syntax.
#'
#' @section Flexibility:
#' `as_indicator()` and, by extension, [create()] etc. are intended to be
#' flexible in syntax. The functions are therefore generic and may be extended
#' by you. The standard method to specify an indicator is either by explicit
#' source and name, by the scheme "<source>-<name>" or by the scheme
#' "<source>__<name>".
#'
#' @param source What is the indicator's source? If you create the indicator
#' from other indicators, the source is "composite". You may also specify it by
#' "" or by not mentioning source at all.
#' @param name What is the indicator's name? Within each source, names have to
#' be unique.
#' @param ... Further attributes.
#' @param df You may also provide the information by a data frame. Name and
#' source must be a part of `df` in that case. It is recommended to use a tibble
#' as this prevents common errors such as providing a source by a factor
#' (`data.frame` automatically coerces `character` to `factor`).
#'
#' @import dplyr magrittr tidyr assertthat stringr
#'
#' @export

indicator <- function(name = NULL, source = "",
                      ...,
                      df = NULL) {
  if(is.null(df)) {
    if(is.null(name)) return(structure(list(),
                                       class = c("indicator", class(list()))))
    df <- tibble(name = name, source = source, ...)
  }

  else {
    assert_that("name" %in% colnames(df), "source" %in% colnames(df))
    df <- as_tibble(df)
  }

  df %<>% mutate(source = if_else(source == "", "composite", source)) %>%
    unite(id, -name, remove = FALSE)

  # id is now the identifier as we have already established that id may not be
  # an attribute.

  ret <- list()

  for(i in seq_len(length(unique(df$id)))) {
    tmp_tibble <- df %>% filter(id == unique(df$id)[i])
    ret[[i]] <- tmp_tibble$name
    attributes(ret[[i]]) <- tmp_tibble[1, ] %>% select(-name, -id, -source)
    class(ret[[i]]) <- c(tmp_tibble$source[1], "indicator_el", class(ret[[i]]))
  }

  class(ret) <- c("indicator", class(ret))

  ret
}

#' @rdname indicator
#'
#' @param spec Your specification of an indicator. The schemes "<source>-<name>"
#' and "<source>__<name>" are standard for character but you may enhance
#' character specification or create alternative class specifications. The
#' specifications are overwritten if a name or source is provided.
#'
#' @export

as_indicator <- function(spec = NULL,
                         name = NULL, source = "",
                         ...) {
  UseMethod("as_indicator", spec)
}

#' @export

as_indicator.NULL <- function(spec = NULL,
                              name = NULL, source = "",
                              ...) {
  indicator(name = name, source = source, ...)
}

#' @export

as_indicator.default <- function(spec = NULL,
                                 name = NULL, source = "",
                                 ...) {
  stop("A method for a specification of class ",
       class(spec),
       " could not be found.\n",
       "You may provide a specification by defining the function ",
       "as_indicator.", class(spec), ".\n")
}

#' @export

as_indicator.character <- function(spec = NULL,
                                   name = NULL, source = "",
                                   ...) {
  # The "-"-scheme is easier to read and will therefore be searched first.

  rec_spec <- str_split_fixed(spec, "-", 2)
  rec_spec[rec_spec[,2] == "", ] <-
    str_split_fixed(rec_spec[rec_spec[,2] == "", 1], "__", 2)
  rem_void <- rec_spec[,2] == ""
  rec_spec[rem_void, 2] <- rec_spec[rec_spec[,2] == "", 1]
  rec_spec[rem_void, 1] <- ""
  indicator(df = tibble(name = rec_spec[,2], source = rec_spec[,1],
                        ...))
}

#' Indicator Access Functions
#'
#' These functions provide access to information on the indicators.
#'
#' @param indicator The indicator or indicator element we are interested in.
#'
#' @return The sources of an indicator or an indicator element.
#'
#' @aliases ind_*
#'
#' @export

ind_source <- function(indicator) {
  UseMethod("ind_source")
}

#' @export

ind_source.indicator <- function(indicator) {
  lapply(indicator, ind_source) %>% unlist %>% unique
}

#' @export

ind_source.indicator_el <- function(indicator) {
  ret <- class(indicator)
  el <- which(ret == "indicator_el")
  ret[seq_len(el - 1)]
}
