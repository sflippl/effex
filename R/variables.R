#' Variables
#'
#' Variables with respect to collections correspond to column names with
#' respect to data frames. They contain information on every column in every
#' collection element.
#'
#' @param x an object.
#'
#' @export

is_variable <- function(x) inherits(x, "variable")

#' @rdname is_variable
#'
#' @param ind_name Names of the indicators
#'
#' @param col The columns that belong to the respective indicators.
#'
#' @param df_name If a specific column belongs to a data frame with a name, this
#' column provides the name.
#'
#' @param df_key The key of a specific column/variable.
#'
#' @param key logical. Is a specific variable a key?
#'
#' @param nr_lst integer Internal: gives the list number in which a particular
#' column can be found.
#'
#' @param ... other column with variable specific information. If the
#' information belongs to the indicators, put it in the indicators
#' collection_df.
#'
#' @export

variable <- function(ind_name = character(0),
                     col = character(0),
                     df_name = "",
                     df_key = new_key(),
                     is_key = logical(0),
                     nr_lst = integer(0),
                     ...) {
  if(is.character(df_key)) df_key <- new_key(df_key)
  assertthat::assert_that(is_key(df_key), is.character(df_name))
  ret <- collection_df(
    dplyr::tibble(
      ind_name = ind_name, col = col, df_name = df_name, df_key = df_key,
      is_key = is_key, nr_lst = nr_lst, ...
    ),
    key = c("col", "df_name", "df_key")
  )
  class(ret) <- c("variables", class(ret))
  ret
}

