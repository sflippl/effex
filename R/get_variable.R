#' Get Variable
#'
#' `get_variable()` transforms indicators - promises of data - into variables,
#' i. e. data. You may specify the way create works by the functions
#' `get_raw_variable()`, `tidy()` and `clean()`.
#'
#' @param indicator The indicators that are to be created.
#'
#' @return The final variable
#'
#' @export

get_variable <- function(indicator) {
  UseMethod("get_variable")
}

#' @export

get_variable.default <- function(indicator) {
  get_variable(as_indicator(indicator))
}

#' @export

get_variable.indicator <- function(indicator) {
  get_raw_variable(indicator) %>%
    tidy %>%
    clean
}

#' @describeIn get_variable The raw variable with minimal cleaning and without
#' ids.
#'
#' @export

get_raw_variable <- function(indicator) {
  UseMethod("get_raw_variable")
}

#' @export

get_raw_variable.indicator <- function(indicator) {
  ret <- list()
  for(i in seq_len(length(indicator))) {
    ret[[i]] <- get_raw_variable(indicator[[i]])
    attr(ret[[i]], "indicator") <- indicator[[i]]
    class(ret[[i]]) <- c(ind_source(indicator[[i]]), class(ret[[i]]))
  }
  class(ret) <- c("raw_variable", class(ret))
  ret
}

#' @export

get_raw_variable.default <- function(indicator) {
  stop("No function for ", ind_source(indicator), " provided.")
}

#' @describeIn get_variable The tidied variable with an id. Every column must have
#' class `variable_col` with sub-classes `c(<source>__<name>, <source>)`.
#'
#' @export

tidy <- function(raw_var) {
  UseMethod("tidy")
}

#' @export

tidy.raw_variable <- function(raw_var) {
  ret <- variable()
  for(i in seq_len(length(raw_var))) {
    tmp <- tidy(raw_var[[i]])
    ids_ret <- ids(ret)
    ids_tmp <- ids(tmp)
    ids_ret_tmp <- ids_ret[ids_ret %in% ids_tmp]
    if(length(ids_ret_tmp) == 0) {
      tmp_in_ret <- tibble()
      for(nam in ids_tmp) {
        tmp_in_ret[[nam]] <-
          structure(rep(NA, nrow(ret)), class = class(tmp[[nam]]))
      }
      ret %<>% bind_cols(variable(id = tmp_in_ret))
      ids_ret_tmp <- ids_tmp
    }
    ret %<>% full_join(tmp, by = ids_ret_tmp)
  }
  ret
}

#' @describeIn get_variable The final, cleaned variable.
#'
#' @export

clean <- function(var) {
  UseMethod("clean")
}

#' @export

clean.variable <- function(var) {
  for(col in colnames(var)) {
    var[[col]] <- structure(clean(var[[col]]),
                            class = c(class(var[[col]])[1],
                                      class(clean(var[[col]])),
                                      "variable_col"))
  }
  var
}

#' @export

clean.variable_col <- function(var) {
  var
}
