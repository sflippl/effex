#' Variables
#'
#' A variable is a data frame of indicator variables, i. e. every column is
#' associated with an indicator and every row is associated with an id.
#'
#' @export

variable <- function(id = character(0), ...) {
  ret <- tibble(...)
  assert_that(unique(length(id)) == length(id), length(id) == nrow(ret))
  ret$id = structure(id, class = c("id", class(id)))
  for(i in seq_len(ncol(ret))) {
    class(ret[, i]) <- c(class(ret[, i]), "variable_col")
  }
  class(ret) <- c("variable", class(ret))
  ret
}
