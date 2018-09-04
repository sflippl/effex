#' Construct mapping for effex
#'
#' Effex mappings describe how indicators are mapped to certain statistics.

fx <- function(...) {
  lst <- rlang::dots_list(...)
  if(length(lst) == 0) return(structure(lst, class = c("fx", "list")))
  assertthat::assert_that(names(lst)[1] != 1,
                          msg = "First argument needs to be named.")
  lst_names <- names(lst)

}
