#' Effex: Select Indicators
#'
#' This effex sets up a shiny element that allows you to select among different
#' indicators according to their name.
#'
#' @export

effex_selectIndicator <- function(indicator, mapping, inputId) {
  assertthat::assert_that("name" %in% names(mapping))
  shiny::selectInput(
    inputId = inputId,
    label = "Select an indicator.",
    choices = indicator$var_name %>%
      magrittr::set_names(eval_tidy(mapping[["name"]], data = indicator))
  )
}
